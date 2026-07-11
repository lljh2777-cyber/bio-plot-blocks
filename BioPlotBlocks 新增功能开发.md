## 一：新增统一数据导入功能

请在现有 BioPlotBlocks 项目中新增“Import Data / 导入数据”功能，使用户可以导入真实数据，而不是只能使用软件内置示例数据 `df`。

### 一、功能目标

支持用户从本地导入以下格式：

- `.csv`
- `.tsv`
- `.txt`
- `.xlsx`
- `.rds`
- `.RData`
- `.rda`

导入完成后，用户应能够：

1. 查看数据基本信息。
2. 预览数据内容。
3. 选择需要使用的数据对象。
4. 检查列类型和数据质量。
5. 将选中的数据绑定为绘图模块可使用的数据源。
6. 在生成的 R 代码中使用明确的数据对象名称，而不是永久依赖内置的 `df`。

### 二、实现路径

在顶部工具栏增加独立按钮：

```text
Import Data
```

不要与现有的以下功能混淆：

- `Import R`：导入 R/ggplot2 代码。
- 项目文件夹按钮：打开 BioPlotBlocks 项目 JSON。
- `Import Data`：导入数据表或 R 数据对象。

建议将导入流程设计为分步向导：

```text
选择文件
→ 解析文件
→ 数据预览
→ 对象或数据表选择
→ 列类型确认
→ 数据质量检查
→ 保存为项目数据源
```

对于文本文件，应支持配置：

- 分隔符；
- 文件编码；
- 是否包含表头；
- 缺失值表示；
- 引号字符；
- 小数点格式；
- 跳过行数。

对于 Excel 文件，应支持：

- 列出工作表；
- 选择工作表；
- 选择数据区域；
- 指定首行为列名。

### 三、内部数据管理

导入后的数据不要只存放在临时前端状态中，应注册到统一的数据源管理器，例如：

```json
{
  "id": "dataset_001",
  "name": "deseq2_results",
  "sourceType": "csv",
  "originalFileName": "DEG_results.csv",
  "objectType": "data.frame",
  "rows": 18432,
  "columns": 8,
  "status": "ready"
}
```

绘图模块应通过数据源 ID 获取数据，不要直接依赖固定变量名。

生成 R 代码时，可以根据内部数据源名称生成：

```r
ggplot(
  data = deseq2_results,
  aes(x = log2FoldChange, y = -log10(padj))
)
```

### 四、界面要求

导入成功后显示数据摘要：

```text
数据名称：deseq2_results
对象类型：data.frame
行数：18,432
列数：8
数值列：5
分类列：2
缺失值：34
重复行：2
```

数据预览至少显示：

- 前 20–50 行；
- 列名；
- 推断类型；
- 缺失值数量；
- 唯一值数量。

用户可以重命名数据源，但不得直接修改原文件。

### 五、注意事项

- 不应默认修改用户原始数据。
- 不应在导入阶段自动删除缺失值或重复值。
- 不应静默转换数据类型。
- 所有自动转换必须可见、可撤销。
- 对超大文件应采用分页、抽样预览或延迟加载，避免一次性渲染全部数据。
- 应限制文件大小，并显示明确错误信息。
- 数据导入失败时，应说明是格式、编码、依赖、对象类型还是内存问题。

### 六、验收标准

- 可以正常导入 CSV、TSV 和 RDS。
- Excel 可以选择工作表。
- 数据导入后能够被散点图、箱线图和火山图模块使用。
- 重新打开项目后，能够恢复数据源信息或提示重新链接原始文件。
- 导入错误不会导致整个应用崩溃。
- 当前内置示例数据仍可作为演示数据，但应明确标记为“示例数据”。

---

## 二：新增 `.rds` 数据对象导入功能

请在 BioPlotBlocks 中新增对 `.rds` 文件的原生支持。

### 一、功能目标

允许用户导入由 R 的 `saveRDS()` 保存的单个对象，并判断该对象是否可以直接用于绘图。

优先支持：

- `data.frame`
- `matrix`
- `tibble`
- 简单列表形式的数据表

后续可扩展：

- `DESeqDataSet`
- `SummarizedExperiment`
- `SingleCellExperiment`
- `Seurat`
- `DGEList`
- `GRanges`

### 二、实现路径

使用 R 后端或隔离的 R 执行环境读取：

```r
object <- readRDS(file_path)
```

读取后检查：

```r
class(object)
dim(object)
names(object)
```

根据对象类型执行以下处理：

- `data.frame`：直接注册为数据源。
- `tibble`：保留列类型并作为数据框使用。
- `matrix`：提示转换为数据框。
- 其他对象：进入对象适配器或显示暂不支持。

矩阵转换时提供选项：

```text
是否将行名转换为普通列？
列名：Gene / Feature / RowName / 自定义
```

### 三、对象检查

读取后显示：

```text
文件：deg_results.rds
对象类型：data.frame
R 类：data.frame
尺寸：18,432 × 8
可直接绘图：是
```

如果对象不支持，显示：

```text
对象类型：DESeqDataSet
当前版本无法直接绘图
可提取内容：
- counts
- normalized counts
- colData
- differential results
```

第一版不要求实现所有高级对象提取，但需要保留适配器接口。

### 四、安全要求

- 不执行对象中的函数。
- 不调用对象内保存的表达式。
- 不将对象自动注入全局环境。
- 读取后进行类型白名单检查。
- 对外部指针、连接、环境、函数等对象拒绝直接使用。
- 对超大对象提前检查文件大小，并提示可能的内存风险。

### 五、验收标准

- 单个数据框 RDS 可以成功导入。
- 单个矩阵 RDS 可以转换为数据框。
- 行名可以保留、忽略或转换为列。
- 不支持的对象有明确提示，不导致程序崩溃。
- 导入对象能够进入统一数据源管理器。

---

## 三：新增 `.RData` / `.rda` 多对象浏览器

请为 BioPlotBlocks 新增 `.RData` 和 `.rda` 文件导入功能，并实现多对象浏览和选择界面。

### 一、功能目标

`.RData` 和 `.rda` 可能包含多个 R 对象。软件不得直接选择第一个对象，也不得将对象加载到全局环境。

导入后应：

1. 在隔离环境中加载文件。
2. 列出所有对象。
3. 显示对象名称、类型和尺寸。
4. 标记是否可以直接用于绘图。
5. 允许用户选择一个或多个对象注册为数据源。

### 二、实现路径

在 R 端使用隔离环境：

```r
temp_env <- new.env(parent = emptyenv())
object_names <- load(file_path, envir = temp_env)
```

遍历对象：

```r
lapply(object_names, function(name) {
  object <- get(name, envir = temp_env)
  list(
    name = name,
    class = class(object),
    dimensions = dim(object),
    supported = is.data.frame(object) || is.matrix(object)
  )
})
```

不得使用：

```r
load(file_path)
```

直接写入全局环境。

### 三、对象浏览器界面

界面示例：

```text
analysis.RData

☑ deg
  data.frame
  18,432 × 8
  可直接绘图

☐ expression_matrix
  matrix
  20,000 × 12
  需要转换

☐ sample_info
  data.frame
  12 × 6
  可直接绘图

○ fit
  lm
  当前不支持

○ custom_function
  function
  禁止导入
```

用户选择对象后，可以：

- 保留原对象名称；
- 重命名；
- 注册为独立数据源；
- 对矩阵执行宽表或长表转换。

### 四、类型白名单

第一版允许直接导入：

- `data.frame`
- `matrix`
- `tibble`

第一版拒绝直接导入：

- `function`
- `environment`
- `connection`
- `externalptr`
- `formula`
- 未知引用对象

复杂生信对象应显示为“可识别但尚未支持”，不要误报为普通数据框。

### 五、注意事项

- `.RData` 与 `.rda` 按同一格式处理。
- 不要假设所有对象具有 `dim()`。
- 不要因为一个对象解析失败而终止整个文件解析。
- 对象名称冲突时，应由数据源管理器自动添加命名空间。
- 用户未选择的对象不应长期驻留内存。
- 项目保存时记录原文件与所选对象名称。

### 六、验收标准

- 可以列出包含多个对象的 `.RData` 文件。
- 可以选择其中的数据框和矩阵。
- 不会污染全局 R 环境。
- 不支持的对象有清晰状态说明。
- 用户能够同时导入 `deg` 和 `sample_info` 为两个数据源。

---

## 四：新增基础列类型识别与数据质量检查

请在 BioPlotBlocks 中新增确定性的列类型识别和数据质量检查模块。该功能不依赖 AI。

### 一、功能目标

数据导入后，自动识别每一列的基础类型，并为后续列映射和 ggplot2 参数配置提供依据。

需要识别：

- 数值型；
- 整数型；
- 字符型；
- 逻辑型；
- 分类变量；
- 日期；
- 日期时间；
- 基因或特征标识符候选列；
- 常数列；
- 高缺失列；
- 高唯一值文本列。

### 二、实现路径

优先依据原始 R 类型：

```r
class(column)
typeof(column)
is.numeric(column)
is.factor(column)
is.character(column)
```

对于从 CSV 或 Excel 导入的数据，再进行受控推断：

- 是否可完全转换为数值；
- 唯一值占比；
- 非缺失值数量；
- 日期格式匹配；
- 是否仅包含 `TRUE/FALSE`；
- 是否为低基数字符列。

建议生成列元数据：

```json
{
  "name": "padj",
  "storageType": "double",
  "semanticType": "numeric",
  "missingCount": 12,
  "uniqueCount": 14820,
  "min": 0,
  "max": 0.998,
  "validFor": ["x", "y", "size", "continuousColor"]
}
```

### 三、数据质量检查

至少检查：

- 缺失值；
- 无限值；
- `NaN`；
- 重复列名；
- 重复行；
- 空列；
- 常数列；
- 数值列中的异常文本；
- P 值是否超出 `[0,1]`；
- 调整后 P 值是否超出 `[0,1]`；
- 列名是否为空；
- 行名是否可能包含基因标识符。

### 四、用户交互

显示列信息表：

```text
列名              类型       缺失值   唯一值   建议用途
Gene              文本       0        18,432   标签
log2FoldChange    数值       5        17,912   X/Y轴
padj              数值       34       14,820   显著性
Group             分类       0        3        颜色/分面
```

允许用户手动修改推断类型，但修改前必须提示可能影响。

### 五、注意事项

- “字符列”不应自动全部转换为因子。
- 唯一值很多的文本列不适合作为颜色或形状。
- 数值形式的样本编号不一定是连续变量。
- 不应仅根据前几行判断整个列类型。
- 类型转换应记录在项目历史中。
- 数据质量警告不等于自动清洗。

### 六、验收标准

- 能正确区分数值、文本、分类和日期。
- 能发现重复列名和缺失值。
- 能阻止文本列被错误映射到连续型坐标轴。
- 用户可以覆盖类型推断。
- 所有推断均具有可解释依据。

---

## 五：新增生物信息学结果规则识别引擎

请在 BioPlotBlocks 中实现基于规则的生物信息学结果类型识别系统。第一版不得依赖大语言模型。

### 一、功能目标

根据数据列名、列类型、值域和组合特征，识别常见生物信息学结果表，并推荐对应绘图模块。

第一阶段至少支持：

- DESeq2 差异表达结果；
- edgeR 差异表达结果；
- limma 差异分析结果；
- Seurat marker 结果；
- clusterProfiler 富集结果；
- GSEA 结果；
- 普通表达矩阵；
- 样本元数据表。

### 二、规则库设计

规则应存放在独立配置文件中，不应硬编码在组件内部。

示例：

```yaml
id: deseq2_result
name: DESeq2 differential expression result

required:
  - semantic: effect_size
    aliases:
      - log2FoldChange

  - semantic: adjusted_pvalue
    aliases:
      - padj

optional:
  - baseMean
  - lfcSE
  - stat
  - pvalue

recommendedPlots:
  - volcano
  - ma_plot
  - ranked_barplot
```

建议目录结构：

```text
rules/
├── deseq2.yaml
├── edger.yaml
├── limma.yaml
├── seurat_markers.yaml
├── clusterprofiler.yaml
└── gsea.yaml
```

### 三、评分逻辑

不要只输出“匹配/不匹配”，应计算置信度。

可以综合：

- 必需列匹配比例；
- 可选列匹配比例；
- 列类型是否合理；
- 数值范围是否合理；
- 列组合是否具有区分度；
- 是否存在冲突特征。

示例：

```text
DESeq2 result：0.96
edgeR result：0.42
Generic differential result：0.88
```

识别结果应区分：

```text
确定匹配
可能匹配
无法确定
冲突
```

### 四、避免过度识别

具有 `logFC` 和 `FDR` 的表不一定来自 edgeR，也可能来自 limma。因此应同时输出：

- 具体来源推断；
- 通用语义类型。

例如：

```json
{
  "specificType": "edgeR_or_limma",
  "generalType": "differential_expression_result",
  "confidence": 0.81
}
```

通用语义识别应优先于强行判断软件来源。

### 五、规则扩展机制

为后续“代码函数—可视化模块映射规则”保留扩展能力：

- 新增 YAML/JSON 规则即可加入新类型；
- 每条规则具有版本号；
- 规则可以声明推荐图；
- 规则可以声明默认列映射；
- 规则可以声明必要的数据转换；
- 规则可以声明兼容的软件包和版本。

### 六、用户界面

显示：

```text
检测结果：差异表达分析结果
可能来源：DESeq2
置信度：96%

已识别：
Gene → 基因标签
log2FoldChange → 效应值
padj → 调整后 P 值
baseMean → 平均表达量
```

用户必须可以修改识别结果。

### 七、验收标准

- 标准 DESeq2 结果能够被准确识别。
- 标准 edgeR 和 limma 结果能够被区分或标记为候选。
- 非标准普通数据表不会被强行识别为生信结果。
- 新增规则不需要修改绘图组件核心代码。
- 识别结果包含置信度和匹配依据。

---

## 六：新增列名同义词与语义匹配系统

请为 BioPlotBlocks 建立列语义标准和同义词匹配机制，用于识别不同软件产生的非统一列名。

### 一、功能目标

将不同列名映射到统一语义，例如：

```text
log2FoldChange
logFC
avg_log2FC
LFC
effect_size
```

统一理解为：

```text
effect_size
```

将：

```text
padj
FDR
adj.P.Val
p_val_adj
qvalue
```

统一理解为：

```text
adjusted_pvalue
```

### 二、语义词典

建立独立词典，例如：

```yaml
effect_size:
  aliases:
    - log2FoldChange
    - logFC
    - avg_log2FC
    - LFC
    - fold_change
    - foldchange

adjusted_pvalue:
  aliases:
    - padj
    - FDR
    - adj.P.Val
    - p_val_adj
    - qvalue

feature_label:
  aliases:
    - Gene
    - gene
    - symbol
    - gene_name
    - feature
    - ID
```

### 三、标准化流程

列名匹配前执行：

- 转换为小写；
- 去除前后空格；
- 统一点号、下划线和短横线；
- 处理大小写差异；
- 处理常见前后缀；
- 保留原始列名用于显示和生成代码。

例如：

```text
Adj.P.Val
adj_p_val
ADJ-P-VAL
```

均可进入同一候选集合。

### 四、模糊匹配

模糊匹配只作为候选，不应直接确认。

可依据：

- 编辑距离；
- token 相似度；
- 前缀和后缀；
- 数据值域；
- 关联列是否存在。

例如列名 `adjusted_p` 可以推断为调整后 P 值，但必须结合其数值是否主要位于 `[0,1]`。

### 五、冲突处理

如果一个列名可能有多个语义，显示候选：

```text
列：score

可能含义：
1. enrichment_score，置信度 0.61
2. significance_score，置信度 0.38
3. 未知，置信度 0.01
```

不得静默选择。

### 六、用户自定义词典

允许用户将自己的列名映射保存为项目级规则，例如：

```text
gene_change → effect_size
significance_score → adjusted_pvalue
```

项目级规则优先于内置模糊匹配，但不得覆盖软件核心词典。

### 七、验收标准

- 常见 DESeq2、edgeR、limma 和 Seurat 列名可以映射到统一语义。
- 大小写和分隔符差异不会导致识别失败。
- 模糊匹配不会未经确认直接用于正式绘图。
- 原始列名保持不变。
- 用户可以添加自定义别名。

---

## 七：新增列映射与绘图角色配置功能

请在 BioPlotBlocks 中实现“列 → ggplot2 美学角色”的映射界面。

### 一、功能目标

允许用户将导入数据中的列映射到：

- X；
- Y；
- Color；
- Fill；
- Shape；
- Size；
- Alpha；
- Label；
- Group；
- Facet row；
- Facet column。

对于生物信息学专用图，还应支持语义角色：

- Gene；
- log2FC；
- P value；
- Adjusted P value；
- Mean expression；
- Pathway description；
- Gene ratio；
- Enrichment score；
- Sample；
- Group。

### 二、实现路径

每个绘图模块声明自己的映射规范。

例如火山图：

```json
{
  "plotType": "volcano",
  "requiredMappings": [
    "effect_size",
    "significance"
  ],
  "optionalMappings": [
    "feature_label",
    "category",
    "mean_expression"
  ]
}
```

箱线图：

```json
{
  "plotType": "boxplot",
  "requiredMappings": [
    "x",
    "y"
  ],
  "optionalMappings": [
    "fill",
    "color",
    "facet"
  ]
}
```

### 三、自动建议与用户确认

规则引擎识别后，可以预填：

```text
X：log2FoldChange
Y：padj
Label：Gene
Color：未设置
```

但界面必须标注为“建议映射”，用户确认后才转为正式映射。

### 四、兼容性检查

映射时检查：

- 连续坐标轴通常要求数值列；
- Shape 不适合高基数列；
- 离散颜色不适合几千个类别；
- Size 通常要求数值列；
- Facet 不适合唯一值过多的列；
- Label 应避免默认标注全部数据点。

不合理映射应警告，而不是直接禁止所有高级用法。

### 五、生成代码

列映射应直接进入代码生成器：

```r
ggplot(
  data = deg_results,
  aes(
    x = log2FoldChange,
    y = -log10(padj),
    color = regulation,
    label = Gene
  )
)
```

列名不是合法 R 标识符时，应使用反引号：

```r
aes(x = `log2 fold change`)
```

不要擅自重命名用户列。

### 六、映射保存

列映射应保存在项目文件中：

```json
{
  "datasetId": "dataset_001",
  "plotId": "plot_004",
  "mapping": {
    "x": "log2FoldChange",
    "y": "padj",
    "label": "Gene"
  }
}
```

### 七、验收标准

- 用户可以手动选择 X、Y、颜色和标签列。
- 自动识别结果可以预填映射。
- 错误类型映射会出现解释性警告。
- 映射变化会实时更新 ggplot2 代码和图形预览。
- 保存并重新打开项目后映射仍然存在。

---

## 八：新增统一 Data Understanding Object

请在 BioPlotBlocks 的数据导入、规则识别、列映射和绘图模块之间加入统一的“数据理解对象”，避免各模块直接相互依赖。

### 一、功能目标

任何导入的数据都应被转换为统一的中间描述对象。该对象用于保存：

- 数据源信息；
- 基础对象类型；
- 列元数据；
- 语义识别结果；
- 生信结果类型；
- 置信度；
- 数据质量警告；
- 推荐图形；
- 推荐列映射；
- 用户确认状态。

### 二、建议数据结构

```json
{
  "datasetId": "dataset_001",
  "name": "deseq2_results",
  "source": {
    "type": "csv",
    "fileName": "DEG_results.csv"
  },
  "object": {
    "type": "data.frame",
    "rows": 18432,
    "columns": 8
  },
  "detectedType": {
    "generalType": "differential_expression_result",
    "specificType": "DESeq2_result",
    "confidence": 0.96
  },
  "columns": [
    {
      "name": "Gene",
      "dataType": "character",
      "semanticType": "feature_label",
      "confidence": 0.93
    },
    {
      "name": "log2FoldChange",
      "dataType": "numeric",
      "semanticType": "effect_size",
      "confidence": 1.0
    },
    {
      "name": "padj",
      "dataType": "numeric",
      "semanticType": "adjusted_pvalue",
      "confidence": 1.0
    }
  ],
  "suggestedPlots": [
    "volcano",
    "ma_plot"
  ],
  "suggestedMapping": {
    "x": "log2FoldChange",
    "significance": "padj",
    "label": "Gene"
  },
  "warnings": [
    "34 missing values in padj"
  ],
  "confirmedByUser": false
}
```

### 三、模块边界

应形成以下流程：

```text
Data Parser
→ Type Detector
→ Rule Engine
→ Data Understanding Object
→ Mapping UI
→ Plot Template Engine
→ ggplot2 Code Generator
```

绘图模块不得自行重新识别数据类型。

AI 模块不得直接操作 ggplot2 代码，而应更新或建议修改 Data Understanding Object。

### 四、版本控制

该对象应包含 schema 版本：

```json
{
  "schemaVersion": "1.0"
}
```

后续升级时应支持旧项目迁移。

### 五、注意事项

- 自动推断和用户确认应分别保存。
- 不要覆盖原始识别结果，应保留用户修改记录。
- 置信度需要针对具体字段保存，而不是只有一个总分。
- 数据理解对象不应保存完整大数据内容，只保存元数据和引用。
- 项目 JSON 中保存数据引用、映射和规则结果，而不是无条件嵌入全部数据。

### 六、验收标准

- 数据导入、识别和绘图模块通过统一对象通信。
- 更换绘图模块时不需要重新解析原始文件。
- 用户修改映射后可以记录覆盖关系。
- 后续增加 Python、Plotly 或 ComplexHeatmap 时，可以继续使用该对象。
- 规则引擎和 AI 辅助模块可以独立替换。

---

## 九：新增可选 AI 辅助识别功能

请在 BioPlotBlocks 中新增可选的 AI 辅助识别模块，但 AI 只能作为规则引擎无法确定时的后备能力，不得替代确定性规则系统。

### 一、功能目标

AI 主要处理以下情况：

- 非标准列名；
- 用户自定义缩写；
- 数据表缺少典型列名；
- 用户使用自然语言描述数据；
- 多个规则结果难以区分；
- 用户希望通过自然语言完成列映射。

示例：

```text
gene_change
significance_score
expression_difference
```

AI 可以建议：

```text
gene_change 可能是效应值
significance_score 可能是 P 值或调整后 P 值
```

### 二、触发条件

只有满足以下条件之一时才调用 AI：

- 最高规则置信度低于设定阈值；
- 两个候选规则分数接近；
- 用户主动点击“AI 辅助识别”；
- 用户输入自然语言要求。

不得对每次数据导入自动调用 AI。

### 三、发送给 AI 的内容

默认只发送必要的最小信息：

- 列名；
- 基础列类型；
- 少量经过脱敏的示例值；
- 值域摘要；
- 缺失比例；
- 已有规则候选。

不要默认发送：

- 完整数据文件；
- 全部样本信息；
- 患者身份信息；
- 用户未授权的原始数据。

调用前显示明确的数据范围说明，并获得用户同意。

### 四、AI 输出格式

要求 AI 返回结构化 JSON，不直接返回自由文本代码：

```json
{
  "columnSuggestions": [
    {
      "column": "gene_change",
      "semanticType": "effect_size",
      "confidence": 0.78,
      "reason": "Values include positive and negative changes"
    }
  ],
  "datasetTypeCandidates": [
    {
      "type": "differential_expression_result",
      "confidence": 0.74
    }
  ]
}
```

AI 建议必须经过本地 schema 校验。

### 五、用户确认

界面显示：

```text
AI 建议，不会自动应用

gene_change → log2FC / effect size，置信度 78%
significance_score → adjusted P value，置信度 64%
```

用户可以：

- 接受；
- 修改；
- 拒绝；
- 保存为本地规则。

AI 不得自动生成最终科研结论，也不得静默更改数据。

### 六、规则优先级

建议优先级：

```text
用户已确认映射
> 项目自定义规则
> 官方确定性规则
> 用户历史规则
> AI 建议
> 默认未知
```

AI 建议不得覆盖已确认映射。

### 七、可重复性

项目中记录：

- 是否调用 AI；
- 使用的模型；
- 调用时间；
- 输入摘要；
- 输出建议；
- 用户最终选择。

正式生成 ggplot2 代码时，只使用用户确认后的映射，不直接依赖模型的临时输出。

### 八、失败处理

当 AI 服务不可用时：

- 数据导入功能仍然正常工作；
- 规则识别仍然正常工作；
- 用户仍可手动映射；
- 不得阻断绘图流程。

### 九、验收标准

- AI 功能可以完全关闭。
- 无 AI 时软件仍具有完整基础能力。
- AI 只在低置信度或用户主动请求时触发。
- 发送数据前有明确授权。
- AI 输出经过结构校验和用户确认。
- 最终生成结果具有可重复、可追踪的映射记录。

---

# 推荐开发顺序

建议不要同时实现全部功能，而是按以下顺序开发。

## 第一阶段：形成可用闭环

1. CSV、TSV 数据导入。
2. 基础类型识别。
3. 数据预览和质量检查。
4. 手动列映射。
5. 将映射转换为 ggplot2 代码。
6. 保存数据源和映射配置。

这一阶段完成后，用户已经可以用真实数据绘图。

## 第二阶段：支持 R 用户工作流

1. `.rds` 导入。
2. `.RData` / `.rda` 对象浏览器。
3. 矩阵转数据框。
4. 行名转普通列。
5. 多数据源管理。

## 第三阶段：建立生信垂直能力

1. 生信结果规则引擎。
2. 列语义词典。
3. DESeq2、edgeR、limma、Seurat 和富集结果模板。
4. 推荐绘图和默认映射。
5. 用户自定义规则。

## 第四阶段：加入 Agent 和 AI

1. 自然语言选择绘图模块。
2. 非标准列名辅助识别。
3. 用户确认与反馈沉淀。
4. 将确认结果保存为规则。
5. 支持开发者根据“规则 + Agent”新增模块。

整体架构应坚持以下原则：

```text
规则负责确定性
AI负责不确定性
用户负责最终确认
代码生成器负责忠实表达
```

AI 不是第一版的必要前提。第一版最重要的是完成稳定、可解释、可复现的数据导入、识别、映射和代码生成闭环。