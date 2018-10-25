import os

content = []

with open('tracklist.txt') as f:
    content = f.readlines()

prefix = "tracks"

if not os.path.exists(prefix):
    os.makedirs(prefix)

content = [x.strip() for x in content]

for line in content:
    directory_name, file_name = line.split('/')
    file, extension = file_name.rsplit('.', 1)

    if extension == 'mp4':
        directory_name = '1mp4'

    if not os.path.exists(os.path.join(prefix, directory_name)):
        os.makedirs(os.path.join(prefix, directory_name))

    with open(os.path.join(os.path.join(prefix, directory_name, file_name)), 'w') as temp_file:
        temp_file.write("poggers")
