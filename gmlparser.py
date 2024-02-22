#read libraries
import xml.etree.ElementTree as ET
import numpy as np
import csv
import json
import urllib.request
from scipy.spatial import ConvexHull


 #define Funktion to calculate the surface area
def PolyArea(x,y):
    return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))

# open json List of all LOD2 data raster cells gml files
with open('/Users/matthiassoot/Nextcloud/Habil/LOD2-Modelle/Qgis-Test/Skript/lgln-opengeodata-lod2.geojson', 'r') as f:
    data = json.load(f)
# Define header of table

#for test reasons only iterate over the first 10 cells

opener = urllib.request.build_opener()
opener.addheaders = [('User-agent', 'Mozilla/5.0')]
urllib.request.install_opener(opener)
for s in range(7700,len(data['features']),100):
    list1=['id','DQD','DQB','DQL','GEME','AH','MTH','FH','RT','FU','HO','GEMN','HAUS','LABE', 'GRUF','VOLU']
    for k in range(0,100):
    #iterate through all gml files available for lower saxony
    #for k in range(0,len(data['features'])):
        #create header for bot security issues
        #define url to download
        remote_url = data['features'][(s+k)]['properties']['CityGML']
        #print(data['features'][k]['properties']['CityGML'])
        #over-write the gml file on drive. Write in home directory
        local_file = 'local_copy.gml'
        urllib.request.urlretrieve(remote_url, local_file)
        #define path to file
        tree = ET.parse('/Users/matthiassoot/local_copy.gml')
        #open gml tree
        root = tree.getroot()
        #set iterator to 0
        i=0
        #iterate through all buildings in gml
        for building in root.iter('{http://www.opengis.net/citygml/building/1.0}Building'):
            #write building id to vector liste
            liste = [building.get('{http://www.opengis.net/gml}id')]
            for stringAttribute in root[i][0].findall('{http://www.opengis.net/citygml/generics/1.0}stringAttribute'):
                #Append all elements to the vector liste
                test = stringAttribute.get('name')
                if test == 'DatenquelleDachhoehe':
                    liste.append(stringAttribute[0].text)
                elif test == 'DatenquelleBodenhoehe':
                    liste.append(stringAttribute[0].text)
                elif test == 'DatenquelleLage':
                    liste.append(stringAttribute[0].text)
                elif test == 'Gemeindeschluessel':
                    liste.append(stringAttribute[0].text)
                elif test == 'AbsoluteHoehe':
                    liste.append(stringAttribute[0].text)
                elif test == 'MittlereTraufHoehe':
                    liste.append(stringAttribute[0].text)
                elif test == 'Firsthoehe':
                    liste.append(stringAttribute[0].text)
            #Check if roof type is given and write it 
            if (root[i][0].find('{http://www.opengis.net/citygml/building/1.0}roofType')!=None):
                liste.append(root[i][0].find('{http://www.opengis.net/citygml/building/1.0}roofType').text)
            else: 
                liste.append('NA')
            #write function    
            liste.append(root[i][0].find('{http://www.opengis.net/citygml/building/1.0}function').text)
            #write measured heigth (usable for validation)
            liste.append(root[i][0].find('{http://www.opengis.net/citygml/building/1.0}measuredHeight').text)
            #Iterate throught the adress
            for adress in root[i][0].findall('{http://www.opengis.net/citygml/building/1.0}address'):
                liste.append(adress[0][0][0][0][1][0].text)
                # if street name and number are given append to vector else NA
                if len(list(adress[0][0][0][0][1]))>1:
                    #print(len(list(adress[0][0][0][0][1])))
                    liste.append(adress[0][0][0][0][1][1][0].text)
                    liste.append(adress[0][0][0][0][1][1][1].text)
                else:
                    liste.append('NA')
                    liste.append('NA')
                #{urn:oasis:names:tc:ciq:xsdschema:xAL:2.0}Locality => Stadt
                #{urn:oasis:names:tc:ciq:xsdschema:xAL:2.0}LocalityName => Gemeindename
                #{urn:oasis:names:tc:ciq:xsdschema:xAL:2.0}Thoroughfare => Straße
                #{urn:oasis:names:tc:ciq:xsdschema:xAL:2.0}ThoroughfareNumber => Hausnummer
                #{urn:oasis:names:tc:ciq:xsdschema:xAL:2.0}ThoroughfareName => Straßenname
            
            flac = 0
            for ground in root[i][0].iter('{http://www.opengis.net/citygml/building/1.0}GroundSurface'):
                
                for pos in ground.iter('{http://www.opengis.net/gml}posList'):
                    koordg = pos.text.split()
                    
                    #print(range(len(koordg),3))
                    x=[]
                    y=[]
                    z=[]
                    for j in range(0,len(koordg)-1,3):
                        x.append(float(koordg[j]))
                        y.append(float(koordg[j+1]))
                        z.append(float(koordg[j+2]))
            flac = flac + PolyArea(x, y)
            liste.append(flac)         

            for wall in root[i][0].iter('{http://www.opengis.net/citygml/building/1.0}WallSurface'):
                for pos in wall.iter('{http://www.opengis.net/gml}posList'):
                    koordg = pos.text.split()
                    
                    #print(range(len(koordg),3))
                    for j in range(0,len(koordg)-1,3):
                        x.append(float(koordg[j]))
                        y.append(float(koordg[j+1]))
                        z.append(float(koordg[j+2]))       
            for roof in root[i][0].iter('{http://www.opengis.net/citygml/building/1.0}RoofSurface'):
                for pos in roof.iter('{http://www.opengis.net/gml}posList'):
                    koordg = pos.text.split()
                    
                    #print(range(len(koordg),3))
                    for j in range(0,len(koordg)-1,3):
                        x.append(float(koordg[j]))
                        y.append(float(koordg[j+1]))
                        z.append(float(koordg[j+2]))
            #Formatting Points 
            coordsall = (np.c_[x,y,z])
            #calculate Volume
            volume = ConvexHull(coordsall).volume
            #add volume to table
            liste.append(volume)
            #Attache liste to list1 and overwrite list1
            list1 = np.c_[list1,liste]
            #next iteration
            i = i+1
            
        
        #write to csv
    file = open('/Users/matthiassoot/Nextcloud/Habil/LOD2-Modelle/Qgis-Test/'+str(s)+'.csv', 'w') 

    with file:
        writer = csv.writer(file)

        for row in list1.transpose():
            writer.writerow(row)
