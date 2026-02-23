# Evolution-of-Mainstream-Music-Billboard-Hot-100
Tracking the evolution of mainstream music (1960-2025). This project scrapes Billboard Hot 100 history and enriches it via MusicBrainz and TheAudioDB APIs. It features high-fidelity, decade-by-decade visual dashboards analyzing genre waves, artist dominance, and chart volatility to map pop culture shifts over time.

## Table of Contents
* [Billboard Hot 100 Timeline Infographic](#billboard-hot-100-timeline-infographic)
* [⚙️ Data Collection & Sources](#️-data-collection--sources)
* [Future Charts & Analysis](#future-charts--analysis) ---

## Billboard Hot 100 Timeline Infographic

Below is a representation of music that appeared on the Billboard Hot 100 over the last 7 decades. The Infographic shows:

* **Top Artist:** The act maintaining the highest cumulative weeks on the chart per year.
* **Chart Churn:** The volume of unique hits versus individual artists entering the charts.
* **Chart Velocity:** A density map of how high songs peak versus their total lifespan on the Billboard.
* **Genres Wave:** Market share evolution of the era's top 3 genres.
* **Genre Longevity:** The distribution of songs lifespans for the most popular genres per decade.

*(Click the image to zoom in and view in high resolution)*

<a href="charts/Billboard_Infographic_Web.png">
  <img src="charts/Billboard_Infographic_Web.png" width="100%" alt="Billboard Hot 100 Evolution Infographic">
</a>

---

## ⚙️ Data Collection & Sources

This project uses a multi-source data pipeline, integrating core historical chart data with dual API metadata enrichment to provide a multi-dimensional view of music history. 

The dataset is constructed from three distinct sources:

1. **Billboard Hot 100 History (Base Data)**
   * **Source:** [utdata/rwd-billboard-data GitHub Repository](https://github.com/utdata/rwd-billboard-data)
   * **Description:** Serves as the foundational timeline of the project, containing the weekly chart performance for every song that entered the Hot 100 from 1960 to the present day. Includes track titles, performers, peak positions, and weeks on the chart.

2. **MusicBrainz API (Core Metadata & Band Relations)**
   * **Source:** [MusicBrainz API](https://musicbrainz.org/doc/MusicBrainz_API)
   * **Description:** A robust R-based 2-step extraction script queries this API for every unique performer found in the Billboard base data. It fetches deeply granular information including:
     * Demographic markers (gender, country, career start/end dates).
     * The top community-voted genre and style tags.
     * A secondary "relations" lookup to extract specific band members, allowing the dataset to differentiate between solo acts, duos, and groups.
   * *Note: To respect the API's strict rate limits, the data extraction script features built-in cache-and-resume logic to save progress locally across multiple sessions.*

3. **TheAudioDB API (Supplemental Enrichment)**
   * **Source:** [TheAudioDB API](https://www.theaudiodb.com/api_guide.php)
   * **Description:** Used as a secondary data enrichment layer to fill in genre gaps and provide additional qualitative attributes (like sonic "Mood"). In the final merging phase, this data acts as a fallback for missing MusicBrainz metadata, resulting in a clean and complete genre map for the visualizations.
