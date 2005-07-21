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
import com.bce.eind.core.profil.ProfilPointProperty;

/**
 * @author kimwerner
 */
public class ProfilPoint implements IProfilPoint
{
  private final HashMap<ProfilPointProperty, Double> m_pointProperties = new HashMap<ProfilPointProperty, Double>();

  public final void addProperty( final ProfilPointProperty pointProperty )
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
    for( final Iterator<ProfilPointProperty> tdkIt = m_pointProperties.keySet().iterator(); tdkIt
        .hasNext(); )
    {
      final ProfilPointProperty tdk = tdkIt.next();
      point.addProperty( tdk );
      try
      {
        if( tdk.isClonable() )
        {
          point.setValueFor( tdk, this.getValueFor( tdk ) );
        }
      }
      catch( ProfilDataException e )
      {
        e.printStackTrace();
      }
    }
    return point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilPoint#getProperties()
   */
  public Collection<ProfilPointProperty> getProperties( )
  {
    return Collections.unmodifiableSet( m_pointProperties.keySet() );
  }

  public final double getValueFor( final ProfilPointProperty pointProperty )
      throws ProfilDataException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht: " + pointProperty.getLabel() );

    return m_pointProperties.get( pointProperty ).doubleValue();
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfilPoint#hasTableData(com.bce.eind.core.profildata.tabledata.TableDataKey)
   */
  public boolean hasProperty( ProfilPointProperty pointProperty )
  {
    return m_pointProperties.containsKey( pointProperty );

  }

  public final void removeProperty( final ProfilPointProperty pointProperty )
  {
    m_pointProperties.remove( pointProperty );
  }

  public final boolean setValueFor( final ProfilPointProperty pointProperty, final double value )
      throws ProfilDataException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new ProfilDataException( "Profileigenschaft existiert nicht" );
    m_pointProperties.put( pointProperty, new Double( value ) );
    return true;
  }

 /* public final boolean isPosition( final double breite, final double hoehe )
      throws ProfilDataException
  {
    final int breitePrecision = ProfilPointProperty.BREITE.getPrecision();
    final int hoehePrecision = ProfilPointProperty.HOEHE.getPrecision();

    final double deltaB = Math.abs( this.getValueFor( ProfilPointProperty.BREITE ) - breite );
    final double deltaH = Math.abs( this.getValueFor( ProfilPointProperty.HOEHE ) - hoehe );
    return ((deltaB < Math.exp( -breitePrecision )) & (deltaH < Math.exp( -hoehePrecision )));
  }*/
}
