# 実験Aの予測結果をExcelに貼り付けるための形式に変換するスクリプトです。
# 適当に作成したので、うまく動かない可能性がありますが、必要に応じて修正してご利用ください。

import os
import re

# --- 1. テーブル構造の定義 (変更なし) ---
NEW_COLUMN_HEADERS = ["none_ldt_hard", "none_ldt_soft", "A1_ut_hard", "A1_ut_soft"]
DATASET_NAMES = [f"DS{i}" for i in range(1, 7)]

def initialize_data_structure():
    tables = {}
    percentages = {"70%": "70% 予測：30%", "50%": "50% 予測：50%", "30%": "30% 予測：70%"}
    for key, header_text in percentages.items():
        table_data = {
            "header": f"学習データ：{header_text}", "column_headers": NEW_COLUMN_HEADERS, "data": {}}
        for ds_name in DATASET_NAMES:
            table_data["data"][ds_name] = {col: "NA" for col in NEW_COLUMN_HEADERS}
        tables[key] = table_data
    return tables

tables_data_global = initialize_data_structure()

def get_target_table_key_from_percentage_str(percentage_str):
    if percentage_str == "0.3": return "30%"
    if percentage_str == "0.5": return "50%"
    if percentage_str == "0.7": return "70%"
    return None

# --- 2. ファイル処理とデータマッピング (★最新形式に対応) ---
def process_summary_directory(dir_path, all_tables_data):
    """
    指定されたディレクトリを処理し、情報を抽出してテーブルデータを更新する。
    サマリーファイルは 'threshold_mode' と 'best_pmae' を列に持つ形式を前提とする。
    """
    dir_name = os.path.basename(dir_path)
    match = re.match(r"^(DS[1-6])_([a-zA-Z0-9_]+)_(\d\.\d+)$", dir_name)
    if not match: return False
    ds_key, method_key, percentage_str = match.groups()
    table_key = get_target_table_key_from_percentage_str(percentage_str)
    if not table_key: return False

    summary_file_path = os.path.join(dir_path, f"{dir_name}_summary.txt")
    if not os.path.exists(summary_file_path): return False

    try:
        with open(summary_file_path, 'r', encoding='utf-8') as f:
            lines = [line.strip() for line in f if line.strip()]
    except Exception as e:
        print(f"ファイル読み込みエラー: {summary_file_path} ({e})")
        return False

    if len(lines) < 2:
        print(f"警告: ファイル '{summary_file_path}' にヘッダーまたはデータ行が不足しています。")
        return False

    # ▼▼▼ 新しいファイル形式の解析ロジック ▼▼▼
    header_line = lines[0]
    headers = header_line.split('\t')

    try:
        # 必要な列のインデックスを取得
        mode_idx = headers.index("threshold_mode")
        pmae_idx = headers.index("top10%_pmae_mean")
    except ValueError:
        print(f"警告: ファイル '{summary_file_path}' のヘッダーが想定形式（'threshold_mode', 'top10%_pmae_mean'を含む）ではありません。")
        return False

    pmae_hard_value = "NA"
    pmae_soft_value = "NA"
    found_data = False

    # データ行（ヘッダーの次から）をループしてhard/softの値を探す
    for data_line in lines[1:]:
        values = data_line.split('\t')
        if len(values) <= max(mode_idx, pmae_idx):
            continue  # 列が足りない不正な行はスキップ

        mode = values[mode_idx]
        pmae = values[pmae_idx]

        if mode == "hard":
            pmae_hard_value = pmae
            found_data = True
        elif mode == "soft":
            pmae_soft_value = pmae
            found_data = True

    # 抽出した値をテーブルにマッピング
    target_table = all_tables_data[table_key]
    hard_col_name = f"{method_key}_hard"
    if hard_col_name in target_table["column_headers"]:
        target_table["data"][ds_key][hard_col_name] = pmae_hard_value

    soft_col_name = f"{method_key}_soft"
    if soft_col_name in target_table["column_headers"]:
        target_table["data"][ds_key][soft_col_name] = pmae_soft_value

    if found_data:
        print(f"情報: ディレクトリ '{dir_name}' を処理しました。 (Hard: {pmae_hard_value}, Soft: {pmae_soft_value})")
        return True
    else:
        print(f"警告: ファイル '{summary_file_path}' から hard/soft データが見つかりませんでした。")
        return False

# --- 3. Excel貼り付け用に出力 (変更なし) ---
def print_tables_for_excel(all_tables_data):
    output_lines = []
    def natural_sort_key(ds_name_str):
        return [int(text) if text.isdigit() else text.lower() for text in re.split(r'([0-9]+)', ds_name_str) if text]
    sorted_ds_keys = sorted(DATASET_NAMES, key=natural_sort_key)
    for table_key, table_content in all_tables_data.items():
        output_lines.append(f"\n{table_content['header']}")
        headers_for_print = [""] + table_content["column_headers"]
        output_lines.append("\t".join(headers_for_print))
        for ds_key in sorted_ds_keys:
            row_data = [ds_key]
            for col_header in table_content["column_headers"]:
                row_data.append(str(table_content["data"][ds_key][col_header]))
            output_lines.append("\t".join(row_data))
    print("\n".join(output_lines).lstrip())

# --- 4. メイン実行ブロック (変更なし) ---
if __name__ == "__main__":
    search_directory_input = input("検索を開始するルートディレクトリを入力してください (ENTERのみでカレントディレクトリ): ")
    search_directory = search_directory_input.strip() or "."
    if not os.path.isdir(search_directory):
        print(f"エラー: 指定されたパス '{search_directory}' は存在しません。")
        exit()
    print(f"\n情報: ディレクトリ '{os.path.abspath(search_directory)}' を検索します...")
    processed_count = 0
    for root, dirs, files in os.walk(search_directory):
        for dir_name in list(dirs):
            full_dir_path = os.path.join(root, dir_name)
            if process_summary_directory(full_dir_path, tables_data_global):
                processed_count += 1
        dirs.clear()

    print(f"\n情報: {processed_count} 個の該当ディレクトリを処理しました。")
    print("\n--- Excel貼り付け用データ ---")
    print_tables_for_excel(tables_data_global)