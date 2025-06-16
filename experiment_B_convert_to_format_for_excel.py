import os
import re

# グローバル変数 (変更なし)
MAX_RESOLUTIONS = 7
tables_data_global = {
    "70%": {"header": "学習データ：70% 予測：30%", "rows": []},
    "50%": {"header": "学習データ：50% 予測：50%", "rows": []},
    "30%": {"header": "学習データ：30% 予測：70%", "rows": []}
}

# get_target_table_key_from_filename_percentage 関数 (変更なし)
def get_target_table_key_from_filename_percentage(filename_percentage_str):
    if filename_percentage_str == "0.3":
        return "30%"
    elif filename_percentage_str == "0.5":
        return "50%"
    elif filename_percentage_str == "0.7":
        return "70%"
    else:
        return None

# process_single_summary_file 関数 (変更なし - 前回の回答のものを挿入)
def process_single_summary_file(filepath, all_tables_data_dict):
    filename = os.path.basename(filepath)
    match = re.match(r"^(.*?)_(\d\.\d+)_.*\.txt$", filename, re.IGNORECASE)
    if not match:
        print(f"エラー: ファイル名 '{filename}' ('{filepath}') の形式が正しくありません。期待される形式例: 'DS1_0.3_summary.txt' または 'AnyName_0.X_anySuffix.txt'")
        return False
    dataset_name = match.group(1)
    percentage_str_from_file = match.group(2)
    target_table_key = get_target_table_key_from_filename_percentage(percentage_str_from_file)
    if not target_table_key or target_table_key not in all_tables_data_dict:
        print(f"エラー: ファイル名 '{filename}' ('{filepath}') から抽出されたパーセンテージ '{percentage_str_from_file}' に対応するテーブルキー '{target_table_key}' が定義されていません。")
        return False
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            lines = f.readlines()
    except FileNotFoundError:
        print(f"エラー: ファイル '{filepath}' が見つかりません。")
        return False
    except Exception as e:
        print(f"エラー: ファイル '{filepath}' の読み込み中にエラーが発生しました: {e}")
        return False
    pmae_values_by_resolution = {}
    header_found = False
    pmae_col_idx = -1
    resolution_col_idx = -1
    for line_number, line_content in enumerate(lines):
        cleaned_line = line_content.strip('\ufeff').strip()
        parts = [p.strip().replace('"', '') for p in cleaned_line.split('\t')]
        if not any(parts):
            continue
        if not header_found:
            try:
                if "resolution" in parts and "pmae" in parts:
                    resolution_col_idx = parts.index("resolution")
                    pmae_col_idx = parts.index("pmae")
                    header_found = True
                continue
            except ValueError:
                pass
        if header_found:
            if len(parts) > max(resolution_col_idx, pmae_col_idx):
                try:
                    resolution_val_str = parts[resolution_col_idx]
                    pmae_val_str = parts[pmae_col_idx]
                    if resolution_val_str.lower() == "resolution":
                        continue
                    resolution = int(resolution_val_str)
                    pmae_values_by_resolution[resolution] = pmae_val_str
                except ValueError:
                    print(f"警告: ファイル '{filepath}' の {line_number + 1} 行目 ('{cleaned_line}') で解像度の数値変換エラー。この行をスキップします。")
                except IndexError:
                    print(f"警告: ファイル '{filepath}' の {line_number + 1} 行目 ('{cleaned_line}') は列数が不足しています。この行をスキップします。")
            elif parts:
                 print(f"警告: ファイル '{filepath}' の {line_number + 1} 行目 ('{cleaned_line}') は期待される列数に満たないため、部分的に処理できない可能性があります。")
        elif line_number > 0 and not header_found :
            if line_number == 1:
                 print(f"警告: ファイル '{filepath}' で明確なヘッダー行（'resolution' および 'pmae' を含む）が見つかりません。データの解析が不正確になる可能性があります。")
    if not header_found:
        print(f"エラー: ファイル '{filepath}' で有効なヘッダー（'resolution' および 'pmae' を含む）が見つかりませんでした。処理を中断します。")
        return False
    output_pmaes_for_row = [pmae_values_by_resolution.get(i, "NA") for i in range(1, MAX_RESOLUTIONS + 1)]
    new_table_row = [dataset_name] + output_pmaes_for_row
    all_tables_data_dict[target_table_key]["rows"].append(new_table_row)
    # print(f"情報: ファイル '{filepath}' のデータが、'{target_table_key}' テーブルに '{dataset_name}' として追加されました。") # Excel出力時はこの行のログは冗長になる可能性があるのでコメントアウトも検討
    return True

# print_formatted_tables 関数をExcel貼り付け用に変更
def print_tables_for_excel(all_tables_data_dict):
    """
    全てのテーブルデータをExcelに直接貼り付け可能な形式（タブ区切り）で
    コンソールに出力します。各テーブル内の行はデータセット名で自然順ソートされます。
    """
    column_headers = ["データセット名"] + [f"解像度{j+1}" for j in range(MAX_RESOLUTIONS)]
    
    has_any_data = any(data_dict["rows"] for data_dict in all_tables_data_dict.values())

    # Excel出力の場合、処理結果のログメッセージは標準エラー出力に分けるか、
    # またはこの関数の外でまとめて表示する方が、コピー対象の出力と混ざらないため望ましい場合があります。
    # ここでは、Excel用のデータのみをprintするようにします。
    # ログメッセージは呼び出し元やprocess_single_summary_file内で標準エラーに出力するか、別途管理します。

    output_lines = [] # 出力する行を一旦リストにためる

    if not has_any_data:
        output_lines.append("どのテーブルにも表示するデータがありません。処理対象のファイルを確認してください。")
        # データがない場合でも、各テーブルのヘッダー構造だけは出力する（Excelで枠組みがわかるように）
        for table_key, data_dict in all_tables_data_dict.items():
            output_lines.append(f"\n{data_dict['header']}") # テーブルの主ヘッダー
            output_lines.append("\t".join(column_headers)) # 列ヘッダー
            # output_lines.append("このテーブルにはデータがありません。") # データがないことを示す行（任意）
        print("\n".join(output_lines))
        return

    for table_key, data_dict in all_tables_data_dict.items():
        output_lines.append(f"\n{data_dict['header']}") # テーブルの主ヘッダー
        output_lines.append("\t".join(column_headers)) # 列ヘッダー
        
        if not data_dict["rows"]:
            # output_lines.append("このテーブルにはデータがありません。") # データがないことを示す行（任意）
            pass # データがない場合は列ヘッダーの下は空行となる
        else:
            def natural_sort_key(row_data):
                dataset_name_str = row_data[0] 
                return [int(text) if text.isdigit() else text.lower()
                        for text in re.split(r'([0-9]+)', dataset_name_str) if text]

            # ソート処理
            sorted_rows = sorted(data_dict["rows"], key=natural_sort_key)
            
            for row_content_list in sorted_rows:
                # 全てのセル内容を文字列に変換してから結合
                row_to_print = [str(cell) for cell in row_content_list]
                output_lines.append("\t".join(row_to_print))
    
    # 全ての行を結合して一度に出力（間に余計なprintによる改行が入らないように）
    print("\n".join(output_lines).lstrip()) # 先頭の余分な改行を削除


# --- スクリプトの実行方法 (変更なし - 前回の回答のものを挿入) ---
if __name__ == "__main__":
    # ログメッセージは標準エラーに出力するか、print_tables_for_excel の前にまとめて表示する
    # 例: print("ファイル処理を開始します...", file=sys.stderr) # sysモジュールのimportが必要

    search_directory_input = input("検索を開始するルートディレクトリを入力してください (例: ./data や C:/Users/YourName/Documents ENTERのみでカレントディレクトリ): ")
    
    search_directory = search_directory_input.strip()
    if not search_directory:
        search_directory = "."
        print(f"情報: 入力がなかったため、カレントディレクトリ '{os.path.abspath(search_directory)}' を検索します。") # 情報メッセージ
    
    if not os.path.isdir(search_directory):
        print(f"エラー: 指定されたパス '{search_directory}' が存在しないか、ディレクトリではありません。処理を終了します。")
        exit()

    found_files_to_process = []
    file_pattern_regex = re.compile(r"^(.*?)_(\d\.\d+)_.*\.txt$", re.IGNORECASE)

    # 検索中のメッセージ (ログとして)
    print(f"\n情報: ディレクトリ '{os.path.abspath(search_directory)}' およびそのサブディレクトリ内のファイルを検索しています...")
    for root, dirs, files in os.walk(search_directory):
        for filename in files:
            if file_pattern_regex.match(filename):
                full_path = os.path.join(root, filename)
                found_files_to_process.append(full_path)

    if not found_files_to_process:
        print(f"情報: 指定されたディレクトリ '{os.path.abspath(search_directory)}' 内に、処理対象のファイルが見つかりませんでした。")
        print("情報: ファイル名の形式が 'データセット名_割合_任意の文字列.txt'（例: DS1_0.3_summary.txt）であることを確認してください。")
    else:
        # 発見ファイルログ (任意で表示件数制限)
        print(f"\n情報: 以下の {len(found_files_to_process)} 個の処理対象ファイルが見つかりました:")
        for i, f_path in enumerate(found_files_to_process):
            if i < 20: 
                 print(f"  - {f_path}")
            elif i == 20:
                 print(f"  ... (他 {len(found_files_to_process) - 20} 件のファイル)")
                 break
        print("\n情報: これらのファイルの処理を開始します...")


    processed_count = 0
    # successfully_processed_files = [] # 成功リストはログ出力には直接使わないので省略可
    failed_files = []

    for file_path in found_files_to_process:
        # process_single_summary_file内でエラー/情報ログはprintされる
        if process_single_summary_file(file_path, tables_data_global):
            processed_count += 1
            # successfully_processed_files.append(file_path)
        else:
            failed_files.append(file_path)
    
    # 処理結果サマリーログ
    if processed_count > 0:
        print(f"\n情報: 合計 {processed_count} 個のファイルの処理が正常に完了しました。")
    
    if failed_files:
        print(f"\n警告: 以下の {len(failed_files)} 個のファイルは処理中にエラーが発生またはデータ抽出できませんでした。詳細は上記のログを確認してください:")
        for i, f_path in enumerate(failed_files): # エラーファイルリスト表示
            if i < 10: print(f"  - {f_path}")
            elif i == 10: print(f"  ... (他 {len(failed_files) - 10} 件のエラーファイル)"); break
    
    if not found_files_to_process and processed_count == 0:
         print("情報: 処理対象ファイルが見つからず、データ処理も実行されませんでした。")
    elif found_files_to_process and processed_count == 0 and not failed_files:
        # このケースは、ファイルは見つかったが、中身が空などでデータ行が抽出されなかった場合
        print("\n情報: 処理対象ファイルから有効なデータ行を抽出できませんでした。ファイル内容を確認してください。")
    elif found_files_to_process and processed_count == 0 and failed_files:
         print("\n警告: 処理対象として認識されたファイルがありましたが、いずれも正常に処理できませんでした。")


    # Excel貼り付け用のテーブルデータを出力
    # この関数の出力は標準出力の主要部分となる
    print("\n--- Excel貼り付け用データ ---")
    print_tables_for_excel(tables_data_global)