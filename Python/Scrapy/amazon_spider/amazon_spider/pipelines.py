# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://docs.scrapy.org/en/latest/topics/item-pipeline.html


# useful for handling different item types with a single interface
from itemadapter import ItemAdapter

import sqlite3
import pymongo

class AmazonSpiderPipeline:
    def __init__(self):
        self.create_connection()
        self.create_table()

        self.client = pymongo.MongoClient("mongodb://127.0.0.1:27017/")
        # self.collection = self.client["scrapy-db"].amazon_scifi_books
        self.collection = self.client["scrapy-db"].amazon_crime_thriller_books

    def create_connection(self):
        # self.conn = sqlite3.connect("./data/amazon-scifi-books.db")
        self.conn = sqlite3.connect("./data/amazon-crime-thriller-books.db")
        self.cur = self.conn.cursor()

    def create_table(self):
        self.cur.execute("""CREATE TABLE IF NOT EXISTS amazon_books_data(
                title text,
                author text,
                price text,
                image_link text            
        ) """)

    def process_item(self, item, spider):
        self.store_db(item)

    def store_db(self, item):
        try:
            self.cur.execute("""INSERT INTO amazon_books_data VALUES (?,?,?,?)""", (
                item["book_title"],
                item["book_author"],
                item["book_price"],
                item["book_imagelink"]
            ))
            self.conn.commit()
        except:
            pass

        self.collection.insert({
            "title" : item["book_title"],
            "author" : item["book_author"],
            "price" : item["book_price"],
            "image_link" : item["book_imagelink"]
        })
