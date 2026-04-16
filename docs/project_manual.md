# Transit Accessibility 项目文档（参数手册 + 运行说明）

> 目标：这份文档作为项目的“主操作手册”，集中说明**如何运行**、**参数含义**、**输出目录结构**、**常见问题排查**。  
> 建议与 `README.md` 配合使用：README 偏快速上手，这份文档偏“完整参数说明”。

---

## 1. 项目流程总览

完整 pipeline（单一主入口：`scripts/09_run_pipeline_main.R`）执行顺序：

1. 下载地理边界与 OSM（`run_download_geography`）
2. 标准化出行调查（`run_standardize_surveys`）
3. 聚合 OD 权重（`run_build_od_weights`）
4. GTFS 供给汇总（`run_gtfs_supply_compare`）
5. r5r 计算 OD 出行时间（`run_compute_od_travel_times`）
6. 可达性指标聚合（`run_aggregate_metrics`）
7. 交互式地图生成（`run_make_maps`）

---

## 2. 运行方式

### 2.1 推荐：一键主脚本（可集中改关键参数）

文件：`scripts/09_run_pipeline_main.R`

你只需要修改脚本中的 `pipeline_main`：

```r
pipeline_main <- list(
  config_path = file.path("config", "chicago.yml"),
  source_id = NULL,
  analysis_unit = NULL, # tract / zip / taz
  run_label = NULL,
  force_all = FALSE
)
```

字段含义：

- `config_path`：配置文件路径（相对路径会自动按项目根目录解析）
- `source_id`：调查数据源 ID（为空则用配置默认）
- `analysis_unit`：运行时覆盖分析单元（不改 yaml 也能切换）
- `run_label`：输出标签，会进入 run_id 命名
- `force_all`：是否强制重跑全部步骤

### 2.2 分步脚本

可按 `scripts/01~07` 分步执行，适合调试单个环节。

---

## 3. 配置参数手册（YAML）

以下以 `config/template_city.yml` 为准。

## 3.1 `project`

- `city_id`：城市英文 ID（决定目录名）
- `city_label`：城市显示名（地图标题等）
- `survey_name`：调查名称
- `survey_wave`：调查批次名
- `timezone`：时区（影响路由时间组合）

## 3.2 `survey`

- `source_id`：调查源唯一 ID
- `adapter`：调查适配器名（在 `R/utils_survey_common.R` 注册）
- `zip_path`：原始调查压缩包路径
- `keep_complete_only`：是否仅保留完整样本

## 3.3 `analysis_area`

- `tract_year`：tract 边界年份
- `county_outline_year`：县边界年份
- `zip_zcta_year`：ZIP 模式使用的 ZCTA 年份（**默认 2020，建议保持 2020**）
- `taz_file`：TAZ 模式的外部矢量文件路径
- `taz_id_col`：TAZ 主键字段名
- `taz_name_col`：TAZ 名称字段名
- `counties`：分析区县列表（`state + county`）

> 说明：ZIP 模式下，项目会基于 tract 中心点落入哪个 ZCTA 来建立 tract→ZIP crosswalk。

## 3.4 `geography`

- `analysis_unit`：分析单元（`tract` / `zip` / `taz`）

## 3.5 `osm`

- `download_url`：OSM PBF 下载地址
- `local_pbf_path`：本地 PBF 路径

## 3.6 `study_periods`

每个 period 包含：

- `period_id`
- `period_label`
- `period_start`
- `period_end_exclusive`（右开区间）

## 3.7 `analysis_calendar`

- `weekday_only`：是否仅工作日
- `excluded_dates`：排除日期列表

## 3.8 `od_scenarios`

每个场景包含：

- `scenario_id` / `scenario_label`
- `weekday_only`
- `allowed_travel_dow`
- `transit_only`
- `allowed_purpose_groups`
- `allowed_mode_groups`
- `exclude_same_tract`
- `time_bin_rules`（定义时间窗）

## 3.9 `od_settings`

- `minimum_weight`
- `minimum_weight_share_within_origin`
- `max_destinations_per_origin`

## 3.10 `auxiliary_weights`

- `origin_multiplier_file` / `destination_multiplier_file`
- `origin_id_col` / `destination_id_col`
- `origin_multiplier_col` / `destination_multiplier_col`

用于给 OD 权重加乘子（如人口、就业等）。

## 3.11 `r5r_network`

- `java_memory`
- `overwrite_networks`
- `java_active_processors`

## 3.12 `routing`

- `modes`
- `max_walk_time`
- `walk_speed`（km/h）
- `max_trip_duration`
- `max_rides`
- `max_lts`
- `percentiles`
- `service_area_buffer_m`
- `restrict_to_service_area`
- `origin_chunk_size`
- `date_strategy`
- `sample_n_dates_per_period`
- `n_threads`
- `unreachable_penalty_minutes`
- `routing_windows`（与 OD 场景关联）

## 3.13 `map`

- `default_comparison_id`
- `default_time_window_id`
- `od_top_pairs_max`
- `od_top_pairs_default`
- `comparison_metrics`
- `period_metrics`

## 3.14 `gtfs_feeds`

每个 feed 配置：

- `feed_name`
- `valid_start`
- `valid_end_exclusive`
- `gtfs_files`

## 3.15 `optional_covariates`

- `download_population`：是否下载可选协变量（当前默认不自动下载）

## 3.16 `run_management`

- `run_label`：输出标签（可选）

## 3.17 `run_options`

- `force`
- `force_downloads`
- `force_standardize`
- `force_od`
- `force_gtfs`
- `force_routing`
- `force_metrics`
- `force_maps`

---

## 4. 输出目录与命名规则

### 4.1 核心输出

- Survey 标准化：`data/processed/<city>/survey/<source_id>/`
- 运行结果：`data/processed/<city>/runs/<source_id>/<analysis_unit>/<run_id>/`
- 地图输出：`outputs/<city>/maps/<source_id>/<analysis_unit>/<run_id>/`

### 4.2 Geography 输出（按分析单元隔离）

`data/processed/<city>/geography/<analysis_unit>/`

包含：

- `tracts.gpkg`
- `tract_centroids.gpkg`
- `county_outlines.gpkg`
- `analysis_zones.gpkg`
- `analysis_zone_centroids.gpkg`
- `tract_to_analysis_zone.csv`

### 4.3 run_id 规则

- 有 `run_label`：`<run_label>__<unit>__<sig12>`
- 无 `run_label`：`<unit>__<sig12>`

---

## 5. 常见问题（FAQ）

### Q1：ZIP 模式报错 “CB ZCTA file for 2023 not released”

A：请把 `analysis_area.zip_zcta_year` 设为 `2020`（模板已默认 2020）。

### Q2：切换 analysis_unit 后，某些步骤还在读 tract 目录

A：建议使用 `scripts/09_run_pipeline_main.R` 并在 `pipeline_main$analysis_unit` 明确设置；脚本会统一覆盖运行配置。

### Q3：为什么会出现空输出目录

A：pipeline 结束会执行空目录清理（含 logs）。如果仍有空目录，多数是中断在中途步骤，可重跑或手动清理。

### Q4：如何避免重复重算

A：保持 `run_options.force* = FALSE`，项目会利用已有输出和时间戳 freshness 机制自动跳过未变化步骤。

---

## 6. 建议维护方式

每次新增参数或修改默认行为时，至少同步更新：

1. `config/template_city.yml`
2. 本文档 `docs/project_manual.md`
3. 相关脚本注释（如 `scripts/09_run_pipeline_main.R`）

这样可以保证“配置模板、实现逻辑、文档说明”三者一致。
