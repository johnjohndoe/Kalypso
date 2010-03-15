// SimpleCache.h: Schnittstelle für die Klasse SimpleCache.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_SIMPLECACHE_H__E10560D2_D360_11D9_9697_000C29C56F8A__INCLUDED_)
#define AFX_SIMPLECACHE_H__E10560D2_D360_11D9_9697_000C29C56F8A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <map>

/** Cache von Pointern  */
template<class KEY, class VALUE> class SimpleCache
{
private:
	typedef std::map<KEY, VALUE*> CacheMap;
	CacheMap m_cacheMap;

	/** If true, the values will be detroyed upon destrcution */
	bool m_destroyValues;

public:
	SimpleCache( bool destroyValues ) : m_destroyValues( destroyValues ) {};
	virtual ~SimpleCache() 
	{
		if( m_destroyValues )
		{
			for( CacheMap::iterator cIt = m_cacheMap.begin(); cIt != m_cacheMap.end(); cIt++ )
				delete cIt->second;
		}
	};

public:
	VALUE* Get( const KEY& key )
	{
		CacheMap::iterator cIt = m_cacheMap.find( key );
		if( cIt == m_cacheMap.end() )
			return 0;

		return cIt->second;
	}

	/** Adds an element to the cache and returns the given value for convenience */
	VALUE* Add( const KEY& key, VALUE* value )
	{
		m_cacheMap.insert( CacheMap::value_type( key, value ) );
		return value;
	}
};

#endif // !defined(AFX_SIMPLECACHE_H__E10560D2_D360_11D9_9697_000C29C56F8A__INCLUDED_)
