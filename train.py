# -*- coding: utf-8 -*-
"""
Created on Thu Jun 15 17:26:11 2023

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

playlist_yes = "https://open.spotify.com/playlist/7tv6JBkkDNEGgu9XQKuqRR?si=7efc9ec968834d96"
playlist_no = "https://open.spotify.com/playlist/1h2UQLZYWybkMJG3KJVz4l?si=608df3f2d13e47c7"

yes = get_playlist_tracks(playlist_yes)
no = get_playlist_tracks(playlist_no)
with open('spotify2.csv', 'w', newline='', encoding="utf-8") as f:  # You will need 'wb' mode in Python 2.x
    headers = ["title", "artist", "num_artists", "artists_followers", "artists_popularity",
               "num_sections", "danceability", "energy", "key", "loudness",
               "mode", "speechiness", "acousticness", "instrumentalness",
               "liveness", "valence", "tempo", "duration_ms","fade_in", "fade_out",
               "time_signature", "explicit", "popularity", "genre", "date", "liked"]
    w = csv.DictWriter(f, headers)
    w.writeheader()
    for track in yes:
        #URI
        track_uri_yes = track["track"]["uri"]
        #features
        features = sp.audio_features(track_uri_yes)[0]
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
            followers.append(artist_info["followers"]["total"])
            popularity.append(artist_info["popularity"])
        artist_uri = track["track"]["artists"][0]["uri"]
        artist_info = sp.artist(artist_uri)
        features["artist"] = artist_info["name"]
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
        features["liked"] = "Yes"
        analysis = sp.audio_analysis(track_uri_yes)
        features["num_sections"] = len(analysis["sections"])
        features["fade_in"] = analysis["track"]["end_of_fade_in"]
        features["fade_out"] = analysis["track"]["start_of_fade_out"]
        keys_remove = ["type", "id", "uri", "track_href", "analysis_url"]
        for key in keys_remove:
            features.pop(key, None)
        w.writerow(features)
    for track in no:
        #URI
        track_uri_no = track["track"]["uri"]
        #features
        features = sp.audio_features(track_uri_no)[0]
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
            followers.append(artist_info["followers"]["total"])
            popularity.append(artist_info["popularity"])
        artist_uri = track["track"]["artists"][0]["uri"]
        artist_info = sp.artist(artist_uri)
        features["artist"] = artist_info["name"]
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
        features["liked"] = "No"
        analysis = sp.audio_analysis(track_uri_no)
        features["num_sections"] = len(analysis["sections"])
        features["fade_in"] = analysis["track"]["end_of_fade_in"]
        features["fade_out"] = analysis["track"]["start_of_fade_out"]
        keys_remove = ["type", "id", "uri", "track_href", "analysis_url"]
        for key in keys_remove:
            features.pop(key, None)
        w.writerow(features)