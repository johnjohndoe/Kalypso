/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil.impl.points;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class ProfilPoint implements IProfilPoint
{
  private final HashMap<POINT_PROPERTY, Double> m_pointProperties = new HashMap<POINT_PROPERTY, Double>();

  public final void addProperty( final POINT_PROPERTY pointProperty )
  {
    m_pointProperties.put( pointProperty, new Double( 0 ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilPoint#clonePoint()
   */
  public IProfilPoint clonePoint( )
  {
    final ProfilPoint point = new ProfilPoint();
    for( final Iterator<POINT_PROPERTY> tdkIt = m_pointProperties.keySet().iterator(); tdkIt
        .hasNext(); )
    {
      final POINT_PROPERTY tdk = tdkIt.next();
      point.addProperty( tdk );
      try
      {
        point.setValueFor( tdk, this.getValueFor( tdk ) );
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
   * @see com.bce.eind.core.profil.IProfilPoint#getProperties()
   */
  public Collection<POINT_PROPERTY> getProperties( )
  {
    return Collections.unmodifiableSet( m_pointProperties.keySet() );
  }

  public final double getValueFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht" );

    return m_pointProperties.get( pointProperty ).doubleValue();
  }

  /*
   * (non-Javadoc)
   * 
   * @see com.bce.eind.core.profilinterface.IProfilPoint#hasTableData(com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  public boolean hasProperty( POINT_PROPERTY pointProperty )
  {
    return m_pointProperties.containsKey( pointProperty );

  }

  public final void removeProperty( final POINT_PROPERTY pointProperty )
  {
    m_pointProperties.remove( pointProperty );
  }

  public final boolean setValueFor( final POINT_PROPERTY pointProperty, final double value )
      throws ProfilDataException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht" );
    m_pointProperties.put( pointProperty, new Double( value ) );
    return true;
  }
  public final boolean isEqualPosition( final double breite,final double hoehe) throws ProfilDataException
  {
    final int breitePrecision = ProfilPointProperties.getPointProperty(POINT_PROPERTY.BREITE).getPrecision();
    final int hoehePrecision = ProfilPointProperties.getPointProperty(POINT_PROPERTY.HOEHE).getPrecision();
    
     final double deltaB = Math.abs(this.getValueFor( POINT_PROPERTY.BREITE )- breite );
     final double deltaH = Math.abs(this.getValueFor( POINT_PROPERTY.HOEHE)- hoehe );
     return ((deltaB < Math.exp(-breitePrecision) ) & (deltaH < Math.exp(-hoehePrecision)));
  }
}
