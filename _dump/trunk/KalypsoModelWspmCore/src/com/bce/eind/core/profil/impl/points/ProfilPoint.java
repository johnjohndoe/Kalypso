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
    for( final Iterator<ProfilPointProperty> tdkIt = m_columns.keySet().iterator(); tdkIt.hasNext(); )
    {
      final ProfilPointProperty tdk = tdkIt.next();
      point.addColumn(tdk);
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
  public boolean hasTableData( ProfilPointProperty columnKey )
  {
    return m_columns.containsKey( columnKey );

  }

  private final HashMap<ProfilPointProperty, Double> m_columns = new HashMap<ProfilPointProperty, Double>();

  public final void addColumn( final ProfilPointProperty columnKey )
  {
    m_columns.put( columnKey, new Double( 0 ) );
  }

  public final void removeColumn( final ProfilPointProperty columnKey )
  {
    m_columns.remove( columnKey );
  }

  public final boolean setValueFor( final ProfilPointProperty columnKey, final double value )
      throws ProfilDataException
  {
    if( !(m_columns.containsKey( columnKey )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht" );
    m_columns.put( columnKey, new Double( value ) );
    return true;
  }

  public final double getValueFor( final ProfilPointProperty columnKey ) throws ProfilDataException
  {
    if( !(m_columns.containsKey( columnKey )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht" );

    return m_columns.get( columnKey ).doubleValue();
  }
}
