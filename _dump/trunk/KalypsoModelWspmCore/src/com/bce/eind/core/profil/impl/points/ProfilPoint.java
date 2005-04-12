/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points;

import java.util.HashMap;
import java.util.Iterator;


import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;

/**
 * @author kimwerner
 */
public class ProfilPoint implements IProfilPoint
{

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilPoint#clonePoint()
   */
  public IProfilPoint clonePoint( )
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<ProfilPointProperty> tdkIt = m_pointProperties.keySet().iterator(); tdkIt.hasNext(); )
    {
      final ProfilPointProperty tdk = tdkIt.next();
      point.addProperty(tdk);
      try
      {
        point.setValueFor(tdk,this.getValueFor(tdk));
      }
      catch( ProfilDataException e )
      {
         e.printStackTrace();
      }
    }
    return point;
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilPoint#hasTableData(com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  public boolean hasProperty( ProfilPointProperty pointProperty )
  {
    return m_pointProperties.containsKey( pointProperty );

  }

  private final HashMap<ProfilPointProperty, Double> m_pointProperties = new HashMap<ProfilPointProperty, Double>();

  public final void addProperty( final ProfilPointProperty columnKey )
  {
    m_pointProperties.put( columnKey, new Double( 0 ) );
  }

  public final void removeProperty( final ProfilPointProperty columnKey )
  {
    m_pointProperties.remove( columnKey );
  }

  public final boolean setValueFor( final ProfilPointProperty pointProperty, final double value )
      throws ProfilDataException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht" );
    m_pointProperties.put( pointProperty, new Double( value ) );
    return true;
  }

  public final double getValueFor( final ProfilPointProperty pointProperty ) throws ProfilDataException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht" );

    return m_pointProperties.get( pointProperty ).doubleValue();
  }
}
