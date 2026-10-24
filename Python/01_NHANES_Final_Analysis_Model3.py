# =====================================================================
# 脚本名称: 02_Format_and_Update_Word.py
# 目的: 读取 R 语言输出的原始 CSV，格式化为 SCI 标准，并自动更新 Word 文档
# 依赖库: pandas, python-docx (pip install pandas python-docx)
# =====================================================================

import pandas as pd
from docx import Document
import os


def format_p(p_val):
    """格式化 P 值，符合 SCI 规范 (< 0.001)"""
    if pd.isna(p_val) or p_val == "": return ""
    try:
        p = float(p_val)
        return "< 0.001" if p < 0.001 else f"{p:.4f}"
    except:
        return p_val


def process_and_update_table1():
    print("正在处理并更新 Table 1...")
    # 1. 数据清洗
    df1 = pd.read_csv("Table1_New_Variables_Final.csv")
    output = []
    current_var = ""
    for _, row in df1.iterrows():
        var = row['Variable'].replace('_', ' ')
        if var != current_var:
            output.append([var + ", %", "", "", format_p(row['P_value']), ""])
            current_var = var
        output.append([f"    {row['Level']}", f"{row['Hp_Negative_0_Pct']}", f"{row['Hp_Positive_1_Pct']}", "", ""])

    df_out = pd.DataFrame(output,
                          columns=["Characteristics", "H. pylori Negative (-)", "H. pylori Positive (+)", "P value",
                                   "SMD"])
    df_out.to_csv("Table1_Update_Formatted.csv", index=False, encoding='utf-8-sig')

    # 2. 写入 Word
    doc = Document('Table1_Final.docx')
    table = doc.tables[0]
    for index, row in df_out.iterrows():
        cells = table.add_row().cells
        for i, col in enumerate(df_out.columns):
            cells[i].text = str(row[col]) if pd.notna(row[col]) else ""
    doc.save('Table1_Final_Updated.docx')
    print("✅ Table 1 更新完成 -> Table1_Final_Updated.docx")


def process_and_update_table2():
    print("正在处理并更新 Table 2...")
    # 1. 数据清洗
    df2 = pd.read_csv("Table2_Model3_Results_Final.csv")
    model3_data = {}
    for _, row in df2.iterrows():
        if "Q1" in row['Fiber_Quartile']:
            model3_data['Q1'] = {'or': "1.00 (Reference)", 'p': "-"}
        elif "Q" in row['Fiber_Quartile']:
            q_key = row['Fiber_Quartile'].split()[0]
            model3_data[q_key] = {'or': f"{row['OR']:.3f} ({row['CI_Lower']:.3f}, {row['CI_Upper']:.3f})", 'p': ""}
    model3_data["P for trend"] = {'or': "", 'p': format_p(df2['P_for_trend'].dropna().values[0])}

    # 2. 写入 Word (精准替换 Model 3 区域)
    doc = Document('Table2_Final.docx')
    table = doc.tables[0]
    in_model3 = False
    for row in table.rows:
        cell_text = row.cells[0].text.strip()
        if "Model 3" in cell_text: in_model3 = True; continue
        if in_model3 and "Q" in cell_text:
            q_key = cell_text.split()[0]
            if q_key in model3_data:
                row.cells[1].text = model3_data[q_key]['or']
                row.cells[2].text = model3_data[q_key]['p']
        if "P for trend" in cell_text:
            row.cells[1].text = model3_data["P for trend"]['or']
            row.cells[2].text = model3_data["P for trend"]['p']
            in_model3 = False
    doc.save('Table2_Final_Updated.docx')
    print("✅ Table 2 更新完成 -> Table2_Final_Updated.docx")


def process_and_update_table4():
    print("正在处理并更新 Table 4...")
    # 1. 数据清洗
    df4 = pd.read_csv("Table4_Subgroup_Results_Final.csv")
    var_mapping = {
        "age_group": "Age (years)", "gender": "Gender", "Race_Label": "Race/Ethnicity",
        "bmi_group": "Body mass index", "smoking": "Smoking status", "alcohol_group": "Alcohol consumption",
        "VitC_Cat": "Vitamin C intake", "Energy_Cat": "Total energy intake",
        "Diabetes": "Diabetes", "Hypertension": "Hypertension",
        "Hyperlipidemia": "Hyperlipidemia", "Physical_Activity": "Physical Activity"
    }
    output = []
    current_var = ""
    for _, row in df4.iterrows():
        var = var_mapping.get(row['Subgroup'], row['Subgroup'])
        if var != current_var:
            output.append([var, "", "", format_p(row['P_interaction'])])
            current_var = var
        level = "Smoker" if var == "Smoking status" and str(row['Level']) == "1" else (
            "Non-Smoker" if var == "Smoking status" and str(row['Level']) == "0" else str(row['Level']))
        output.append([f"    {level}", row['OR_95CI'], format_p(row['P_value']), ""])

    df_out = pd.DataFrame(output,
                          columns=["Subgroup", "OR (95% CI) Per quartile increase", "P value", "P for interaction"])

    # 2. 写入 Word (覆盖写入)
    doc = Document('Table4_Final.docx')
    table = doc.tables[0]
    table.cell(0, 1).text = "OR (95% CI) Per quartile increase"

    current_row_idx = 1
    for index, row in df_out.iterrows():
        cells = table.rows[current_row_idx].cells if current_row_idx < len(table.rows) else table.add_row().cells
        for i, col in enumerate(df_out.columns):
            cells[i].text = str(row[col]) if pd.notna(row[col]) else ""
        current_row_idx += 1

    while current_row_idx < len(table.rows):
        for cell in table.rows[current_row_idx].cells: cell.text = ""
        current_row_idx += 1

    # 更新 Notes
    for para in doc.paragraphs:
        if "Notes:" in para.text and "Subgroup analyses" in para.text:
            para.text = "Notes: Subgroup analyses were performed using the fully adjusted model. The ORs represent the risk of H. pylori infection per quartile increase in energy-adjusted dietary fiber intake. OR, odds ratio; CI, confidence interval. P for interaction was calculated by treating the quartiles of energy-adjusted dietary fiber as a continuous variable to test for linear trend interactions."

    doc.save('Table4_Final_Updated.docx')
    print("✅ Table 4 更新完成 -> Table4_Final_Updated.docx")


if __name__ == "__main__":
    process_and_update_table1()
    process_and_update_table2()
    process_and_update_table4()
    print("\n🎉 所有流程执行完毕！最终 Word 文档已生成。")
