#https://www.kaggle.com/paulantoine/light-gbm-benchmark-0-3692
#%%
import numpy as np
import pandas as pd
import lightgbm as lgb
IDIR = 'input/'
print('loading priors')
priors = pd.read_csv(IDIR + 'order_products__prior.csv',
                     dtype={ 'order_id': np.int32,
                            'product_id': np.int16,
                            'add_to_cart_order': np.int16,
                            'reordered': np.int8})

priors.reordered
# In[7]:
print('loading train')
train = pd.read_csv(IDIR + 'order_products__train.csv', dtype={
            'order_id': np.int32,
            'product_id': np.int16,
            'add_to_cart_order': np.int16,
            'reordered': np.int8})

print('loading orders')
orders = pd.read_csv(IDIR + 'orders.csv', dtype={
        'order_id': np.int32,
        'user_id': np.int32,
        'eval_set': 'category',
        'order_number': np.int16,
        'order_dow': np.int8,
        'order_hour_of_day': np.int8,
        'days_since_prior_order': np.float_})

print('loading products')
products = pd.read_csv(IDIR + 'products.csv', dtype={
        'product_id': np.int16,
        'order_id': np.int32,
        'aisle_id': np.uint8,
        'department_id': np.uint8},
        usecols=['product_id', 'aisle_id', 'department_id'])

print('priors {}: {}'.format(priors.shape, ', '.join(priors.columns)))
print('orders {}: {}'.format(orders.shape, ', '.join(orders.columns)))
print('train {}: {}'.format(train.shape, ', '.join(train.columns)))



# In[8]:
prods = pd.DataFrame()
prods['orders'] = priors.groupby(priors.product_id).size().astype(np.int32)
prods['reorders'] = priors['reordered'].groupby(priors.product_id).sum().astype(np.float_)
prods['reorder_rate'] = (prods.reorders / prods.orders).astype(np.float_)
products = products.join(prods, on="product_id")
products.set_index('product_id', drop=False, inplace=True)
print(f'computing proudcts, len:{len(products)}')

# In[9]:
#tmp = priors['reordered'].groupby(priors.product_id).sum()
products.head()
priors.head()


# In[10]:
orders.set_index('order_id', inplace=True, drop=False)
priors = priors.join(orders, on="order_id", rsuffix='_')
priors.drop('order_id_', inplace=True, axis=1)
print("add order info to priors")


# In[11]:
usr = pd.DataFrame()
usr['average_days_between_orders'] = orders.groupby('user_id')['days_since_prior_order'].mean().astype(np.float32)
usr['nb_orders'] = orders.groupby('user_id').size().astype(np.int16)

np.__version__
# In[12]:
users = pd.DataFrame()
users['total_items'] = priors.groupby('user_id').size().astype(np.int16)
users['all_products'] = priors.groupby('user_id')['product_id'].apply(set)
users['total_distinct_items'] = (users.all_products.map(len)).astype(np.float32)
#users = users.join(usr)


# In[13]:
users = users.join(usr)
del usr


# In[14]:
users['average_basket'] = (users.total_items / users.nb_orders).astype(np.float32)
print(f'users shape: {users.shape}')


# In[15]:
priors['user_product'] = priors.product_id + priors.user_id * 100000
print('compute userXproduct f -this is long')


# In[21]:
d = dict()
for row in priors.itertuples():
    z = row.user_product
    if z not in d:
        d[z] = (1, 
                (row.order_number, row.order_id),
                row.add_to_cart_order)
    else:
        d[z] = (d[z][0]+1,
                max(d[z][1], (row.order_number, row.order_id)),
                d[z][2] + row.add_to_cart_order)
print("to dataframe (less memory)")
userXproduct = pd.DataFrame.from_dict(d, orient='index')
del d
print('ok')

# In[20]:
userXproduct.columns = ['nb_orders', 'last_order_id', 'sum_pos_in_cart']
userXproduct.nb_orders = userXproduct.nb_orders.astype(np.int16)
userXproduct.last_order_id = userXproduct.last_order_id.map(
        lambda x: x[1]).astype(np.int32)
userXproduct.sum_pos_in_cart = userXproduct.sum_pos_in_cart.astype(np.int16)
print("done")
del priors

# In[ ]: 
userXproduct
