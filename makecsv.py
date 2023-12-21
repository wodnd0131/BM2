import csv

newFileName = "data_1920.csv"
oldFileName = "transfermarkt_fbref_201920.csv"

dataList = []
with open( oldFileName, 'r', encoding='utf-8' ) as file:
    reader = csv.reader( file, delimiter=';' )
    for row in reader :
        dataList.append( row )

with open( newFileName, 'w', encoding='utf-8', newline='' ) as file:
    writer = csv.writer( file )
    for row in dataList:
        writer.writerow( row )