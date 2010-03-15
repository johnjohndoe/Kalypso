// HierarchyHolder.h: Schnittstelle für die Klasse CHierarchyHolder.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_HIERARCHYHOLDER_H__CED29655_29F5_11D7_B375_00104BB3E525__INCLUDED_)
#define AFX_HIERARCHYHOLDER_H__CED29655_29F5_11D7_B375_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

namespace BCE
{
  template<class T> class CHierarchyHolder  
  {
private:
  typedef std::map<std::string, T*> ItemMap;
      
public:  
  CHierarchyHolder() {};
private:
  CHierarchyHolder( CHierarchyHolder& other );

public:
  ~CHierarchyHolder()
  {
    deleteAllItems();
  }

  T* getExistingInstance( const std::string& name )
  {
    T* item = 0;
    ItemMap::iterator i = m_itemMap.find( name );

    T* result = NULL;
    if( i != m_itemMap.end() )
	    result = i->second;

    return result;
  }; // getExistingInstance

  void deleteAllItems()
  {
    for( ItemMap::const_iterator i = m_itemMap.begin(); i != m_itemMap.end(); i++) 
      delete i->second;
  }; // deleteAllItems

public:
  T& getInstance( const char* name, T.argType& args )
  {
    return getInstance( std::string( name ), args );
  }

  T& getInstance( const std::string& name, T.argType& args )
  {
    T* item = getExistingInstance( name );

    if( !item )
    {            
      if( name == "" )
        item = new T( name, args );
      else
      {
        std::string parentName;
        size_t dotIndex = name.find_last_of('.');
        if( name.length() <= dotIndex )
          parentName = "";
        else 
          parentName = name.substr( 0, dotIndex );

        T& parent = getInstance( parentName );
        
        item = new T( name );
      }	  
        
      m_itemMap[name] = item;
    } // if item

    return *item;
  }; // getInstance

private:
  CHierarchyHolder operator=( CHierarchyHolder& other );

protected:
  CategoryMap m_itemMap;
  }; // class CHierarchyHolder
}; // namespace BCE

#endif // !defined(AFX_HIERARCHYHOLDER_H__CED29655_29F5_11D7_B375_00104BB3E525__INCLUDED_)
