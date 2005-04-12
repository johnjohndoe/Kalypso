package com.bce.eind.core.strang;

import java.util.Collection;
import java.util.LinkedList;


/**
 * @author gernot
 *
 */
public class StrangInfo
{
  private final ProfilInfo[] m_profiles;
  private int m_index;
  private Collection<IStranginfoListener> m_listeners = new LinkedList<IStranginfoListener>();

  public StrangInfo( final ProfilInfo[] profile, final int index )
  {
    m_profiles = profile;
    m_index = index;
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
  public void setIndex( int index )
  {
    m_index = index;
    
    fireIndexChanged();
  }
  
  public ProfilInfo getInfo()
  {
    return m_profiles[m_index];
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
