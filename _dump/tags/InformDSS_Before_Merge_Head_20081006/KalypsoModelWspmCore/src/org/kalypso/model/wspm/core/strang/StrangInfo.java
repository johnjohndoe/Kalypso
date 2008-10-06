package org.kalypso.model.wspm.core.strang;

import java.util.Collection;
import java.util.LinkedList;

import org.kalypso.model.wspm.core.result.IResultSet;



/**
 * @author gernot
 *
 */
public class StrangInfo
{
  private final ProfilInfo[] m_profiles;
  private int m_index;
  private final Collection<IStranginfoListener> m_listeners = new LinkedList<IStranginfoListener>();
  private final IResultSet[] m_results;

  public StrangInfo( final ProfilInfo[] profile, final int index, final IResultSet[] results )
  {
    m_profiles = profile;
    m_index = index;
    m_results = results;
  }
  
  /**
   * @return Returns the index.
   */
  public int getIndex( )
  {
    return m_index;
  }
  
  /**
   * @param index The index to set.
   */
  public void setIndex( final int index )
  {
    if( m_index == index )
      return;
    
    if( !tryChangeIndex() )
      return;
    
    m_index = index;
    
    fireIndexChanged();
  }
  
  private boolean tryChangeIndex( )
  {
    for( final IStranginfoListener l : m_listeners )
    {
      if( !l.onTryChangeIndex( this ) )
        return false;
    }

    return true;
  }

  public ProfilInfo getInfo()
  {
    return m_profiles[m_index];
  }
  
  public IResultSet[] getCurrentResults()
  {
    return m_results;
  }
  
  public int size()
  {
    return m_profiles.length;
  }
  
  public void addStranginfoListener( final IStranginfoListener l )
  {
    m_listeners.add( l );
  }

  public void removeStranginfoListener( final IStranginfoListener l )
  {
    m_listeners.remove( l );
  }
  
  private void fireIndexChanged()
  {
    for( final IStranginfoListener l : m_listeners )
      l.onIndexChanged( this );
  }

  public ProfilInfo[] getInfos( )
  {
    return m_profiles;
  }

  public void setInfo( final ProfilInfo selectedItem )
  {
    for( int i = 0; i < m_profiles.length; i++ )
    {
      if( selectedItem == m_profiles[i] )
      {
        setIndex( i );
        return;
      }
    }
  }
}
