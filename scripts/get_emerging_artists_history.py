# the script fetches historical weekly data of emerging artists from Billboard 
import billboard
import pandas as pd
import time
from datetime import datetime, timedelta

def get_chart_history():
    # Settings
    chart_name = 'emerging-artists'
    start_date_str = '2026-02-28'  # The current chart date from your log
    end_date_str = '2017-08-15'    # Approx start of Emerging Artists chart
    
    # Convert to datetime objects
    current_date = datetime.strptime(start_date_str, '%Y-%m-%d')
    end_date = datetime.strptime(end_date_str, '%Y-%m-%d')
    
    all_data = []
    
    print(f"--- Starting Manual Date Fetch for {chart_name} ---")
    print(f"From {start_date_str} back to {end_date_str}")
    print("This is the robust method. It will take time. Do not close.")

    # Loop backwards by 7 days
    while current_date >= end_date:
        date_str = current_date.strftime('%Y-%m-%d')
        print(f"Fetching week: {date_str}...", end=" ")
        
        try:
            # Fetch specific date directly
            chart = billboard.ChartData(chart_name, date=date_str)
            
            # Check if we actually got data (sometimes dates are off by a day)
            if len(chart) == 0:
                print("Empty (skipped)")
            else:
                print(f"Found {len(chart)} entries.")
                for entry in chart:
                    all_data.append({
                        "chart_date": date_str,
                        "rank": entry.rank,
                        "artist": entry.artist,
                        "title": entry.title,
                        "peak_pos": entry.peakPos,
                        "last_pos": entry.lastPos,
                        "weeks_on_chart": entry.weeks,
                        "is_new": entry.isNew
                    })
                    
        except Exception as e:
            print(f"Error: {e}")

        # Move back 7 days
        current_date -= timedelta(days=7)
        
        # set sleep to avoid IP getting banned by Billboard
        time.sleep(1.5) 

        # Periodic Save (Safety net: saves every 50 weeks so you don't lose everything if it crashes)
        if len(all_data) % 2500 == 0 and len(all_data) > 0:
            print("...Performing safety save...")
            temp_df = pd.DataFrame(all_data)
            temp_df.to_csv("emerging_artists_TEMP.csv", index=False)

    # Save df
    print(f"\nDone! Compiling {len(all_data)} rows...")
    df = pd.DataFrame(all_data)
    
    # Clean and order columns
    cols = ["chart_date", "rank", "artist", "title", "peak_pos", "last_pos", "weeks_on_chart", "is_new"]
    # Filter to ensure columns exist
    df = df[[c for c in cols if c in df.columns]]
    
    # save as csv
    filename = "emerging_artists_history_full.csv"
    df.to_csv(filename, index=False)
    print(f"SUCCESS! Final dataset saved to '{filename}'")

if __name__ == "__main__":
    get_chart_history()