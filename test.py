# -*- coding: utf-8 -*-
"""
Created on Sat Jun 10 18:31:51 2023

@author: charl
"""

import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import csv
import statistics

cid = "xxx"
secret = "xxx"

client_credentials_manager = SpotifyClientCredentials(client_id=cid, client_secret=secret)
sp = spotipy.Spotify(client_credentials_manager = client_credentials_manager)

def get_playlist_tracks(playlist_link):
    playlist_URI = playlist_link.split("/")[-1].split("?")[0]
    results = sp.playlist_tracks(playlist_URI)
    tracks = results['items']
    while results['next']:
        results = sp.next(results)
        tracks.extend(results['items'])
    return tracks

playlist_1 = "https://open.spotify.com/playlist/6d3tfjHoNC6zdwcndSELjI?si=d3467517df6d4bc6"
playlist_2 = "https://open.spotify.com/playlist/7o3sCkAkVLGxsC1KnlwzGr?si=e6da1899254d4795"
playlist_3 = "https://open.spotify.com/playlist/6An8THpMPQjQurQHYvta1v?si=8e1a8f5323944b3f"
playlist_4 = "https://open.spotify.com/playlist/4q5xAX8n4CNZzI1H3jJqKF?si=a1770d4a546f4505"
playlist_5 = "https://open.spotify.com/playlist/5ltdkJnPoxFIwo6WRaRRkE?si=07a0d684536e43ae"
playlist_6 = "https://open.spotify.com/playlist/2Jb4nTUlYxIlOfNVS7CCCm?si=84b0aa5774144494"
playlist_7 = "https://open.spotify.com/playlist/6Q3NEQ2hNrG5adL5cWXdAs?si=131b301f680e4720"
playlist_8 = "https://open.spotify.com/playlist/1MdrdVjUxx4L9jNI6RTwJF?si=d067c6156e5d4f02"
playlist_9 = "https://open.spotify.com/playlist/6hOk3GDOIUCUcNcHWzF5Ci?si=9a416ed662ee46cd"
playlist_10 = "https://open.spotify.com/playlist/0vyTVxJyMbE5IE2XNSSGvz?si=b8aaa5e7966f4259"

playlists = [playlist_1,playlist_2,playlist_3,playlist_4,playlist_5,playlist_6,playlist_7,playlist_8,playlist_9,playlist_10]
with open('test.csv', 'w', newline='', encoding="utf-8") as f:  # You will need 'wb' mode in Python 2.x
    headers = ["track_uri", "title", "artist", "num_artists", "artists_followers", "artists_popularity",
               "num_sections", "danceability", "energy", "key", "loudness",
               "mode", "speechiness", "acousticness", "instrumentalness",
               "liveness", "valence", "tempo", "duration_ms","fade_in", "fade_out",
               "time_signature", "explicit", "popularity", "genre", "date", "liked", "score"]
    w = csv.DictWriter(f, headers)
    w.writeheader()
    nota=1
    for playlist in playlists:
        tracks = get_playlist_tracks(playlist)
        for track in tracks:
            #URI
            track_uri = track["track"]["uri"]
            #features
            features = sp.audio_features(track_uri)[0]
            features["track_uri"] = track_uri
            #Track name
            features["title"] = track["track"]["name"]
            features["explicit"] = track["track"]["explicit"]
            num_artist = len(track["track"]["artists"])
            features["num_artists"] = num_artist
            followers = []
            popularity = []
            for i in range(0, num_artist):
                artist_uri = track["track"]["artists"][i]["uri"]
                artist_info = sp.artist(artist_uri)
                if i == 0:
                    features["artist"] = artist_info["name"]
                followers.append(artist_info["followers"]["total"])
                popularity.append(artist_info["popularity"])
            features["artists_followers"] = statistics.mean(followers)
            features["artists_popularity"] = statistics.mean(popularity)
            try:
                features["genre"] = artist_info["genres"][0]
            except Exception:
                features["genre"] = "desconocido"
                
            #Realese_date
            features["date"] = track["track"]["album"]["release_date"]
            
            #Popularity of the track
            features["popularity"] = track["track"]["popularity"]
            if nota<6:
                features["liked"] = "No"
            else:
                features["liked"] = "Yes"
            features["score"] = nota
            analysis = sp.audio_analysis(track_uri)
            features["num_sections"] = len(analysis["sections"])
            features["fade_in"] = analysis["track"]["end_of_fade_in"]
            features["fade_out"] = analysis["track"]["start_of_fade_out"]
            keys_remove = ["type", "id", "uri", "track_href", "analysis_url"]
            for key in keys_remove:
                features.pop(key, None)
            w.writerow(features)
        nota = nota +1
        