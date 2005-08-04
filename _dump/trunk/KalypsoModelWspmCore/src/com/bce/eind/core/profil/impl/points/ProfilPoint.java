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
import com.bce.eind.core.profil.PointProperty;

/**
 * @author kimwerner
 */
public class ProfilPoint implements IProfilPoint
{
  private final HashMap<PointProperty, Double> m_pointProperties = new HashMap<PointProperty, Double>();

  public final void addProperty( final PointProperty pointProperty )
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
    for( final Iterator<PointProperty> tdkIt = m_pointProperties.keySet().iterator(); tdkIt
        .hasNext(); )
    {
      final PointProperty tdk = tdkIt.next();
      point.addProperty( tdk );
      try
      {
        point.setValueFor( tdk, getValueFor( tdk ) );

      }
      catch( ProfilDataException e )
      {
        return null;
      }
      
    }
    return point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilPoint#getProperties()
   */
  public Collection<PointProperty> getProperties( )
  {
    return Collections.unmodifiableSet( m_pointProperties.keySet() );
  }

  public final double getValueFor( final PointProperty pointProperty )
      throws ProfilDataException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht: " + pointProperty.toString() );

    return m_pointProperties.get( pointProperty ).doubleValue();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfilPoint#hasTableData(com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  public boolean hasProperty( PointProperty pointProperty )
  {
    return m_pointProperties.containsKey( pointProperty );

  }

  public final void removeProperty( final PointProperty pointProperty )
  {
    m_pointProperties.remove( pointProperty );
  }

  public final boolean setValueFor( final PointProperty pointProperty, final double value )
        {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      return false;
    m_pointProperties.put( pointProperty, new Double( value ) );
    return true;
  }

 /* public final boolean isPosition( final double breite, final double hoehe )
      throws ProfilDataException
  {
    final int breitePrecision = PointProperty.BREITE.getPrecision();
    final int hoehePrecision = PointProperty.HOEHE.getPrecision();

    final double deltaB = Math.abs( this.getValueFor( PointProperty.BREITE ) - breite );
    final double deltaH = Math.abs( this.getValueFor( PointProperty.HOEHE ) - hoehe );
    return ((deltaB < Math.exp( -breitePrecision )) & (deltaH < Math.exp( -hoehePrecision )));
  }*/
}
