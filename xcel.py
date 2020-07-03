import xlrd
import os.path
import sqlite3

def chop_index(rows):
    [x.pop(0) for x in rows]
    return rows
    
def cell_value_map(row):
    return [x.value for x in row]

def filter_empty_rows(rows):
    return [x for x in rows if x[1] != '']

def remove_empty_cols(rows):
    [x.pop() for x in rows]
    return rows

def get_rows(file):
    wb = xlrd.open_workbook(file)
    sheet = wb.sheet_by_index(0)
    nrows = sheet.nrows
    prelude_rows = 14
    save = []
    for index in range(prelude_rows, nrows):
       save.append(cell_value_map(sheet.row(index)))
    return remove_empty_cols(remove_empty_cols(chop_index(filter_empty_rows(save))))

fuck = 0
def walk_into_if(path):
    dir = os.listdir(path)
    xce = os.listdir(path)
    dir = [x for x in dir if x.endswith('.jpg') or x.endswith('.JPG')]
    xce = [y for y in xce if y.endswith('.xlsx')]
    save = get_rows(os.path.join(path, xce[0]))
    if fuck == 0:
        if len(save) == len(dir) or len(save) == len(dir) + 1:
            return path
    if fuck == 1:
        if len(save) != len(dir) or len(save) != len(dir) + 1:
            return path
    return False
        
def walk_excel(path):
    save = []
    for root, dirs, files in os.walk(path):
        path = root.split(os.sep)
        for fname in os.listdir(root):
            if fname.endswith('.xlsx'):
                save.append(root)
    return save

def double_walk(path):
    save = walk_excel(path)
    save = [ walk_into_if(x) for x in save ]
    return [ x for x in save if x != False ]

# Willfully Stolen from somewhere
def flatten(l, ltypes=(list, tuple)):
    ltype = type(l)
    l = list(l)
    i = 0
    while i < len(l):
        while isinstance(l[i], ltypes):
            if not l[i]:
                l.pop(i)
                i -= 1
                break
            else:
                l[i:i + 1] = l[i]
        i += 1
    return ltype(l)
# end theft

def sql_row(path):
    dir = os.listdir(path)
    xce = os.listdir(path)
    dir = [x for x in dir if x.endswith('.jpg') or x.endswith('.JPG')]
    xce = [y for y in xce if y.endswith('.xlsx')]
    save = get_rows(os.path.join(path, xce[0]))
    dir = [os.path.join(path, l) for l in dir]
    data = list(zip(save, dir))
    return [flatten(l) for l in data]

def prepare_sql_data(path):
    save = double_walk(path)
    data = [sql_row(x) for x in save]
    return data

conn = sqlite3.connect('test2.db')
c = conn.cursor()

def insert_single_data(dataline):
    c.executemany('INSERT INTO inventory VALUES (?,?,?,?)', dataline)
    
def insert_data(data):
    [insert_single_data(x) for x in data]
    conn.commit()
    
