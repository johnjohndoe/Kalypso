/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil;

import java.util.Collection;
import java.util.HashMap;

/**
 * Kim: Document the interface methods
 * 
 * @author kimwerner
 */
public interface IProfilPoint
{
  /**
   * @param pointProperty
   * @return
   * @throws ProfilDataException
   */
  public double getValueFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException;

  /**
   * @param pointProperty
   * @return
   */
  public boolean hasProperty( final POINT_PROPERTY pointProperty );

  public IProfilPoint clonePoint( );

  public Collection<POINT_PROPERTY> getProperties( );

  public static enum PARAMETER
  {
    LABEL, OPTIONAL, VISIBLE, PRECISION, INTERPOLATION
  };

  public static enum POINT_PROPERTY
  {
    BEWUCHS_AX( "AX", true, true, false, 4 ),

    BEWUCHS_AY( "AY", true, true, false, 4 ),

    BEWUCHS_DP( "DP", true, true, false, 4 ),

    BREITE( "Breite", false, true, true, 4 ),

    HOCHWERT( "Hochwert", true, true, true, 4 ),

    UNTERKANTEBRUECKE( "Brückenunterkante", true, true, false, 4 ),

    OBERKANTEBRUECKE( "Brückenoberkante", true, true, false, 4 ),

    OBERKANTEWEHR( "Wehr", true, true, false, 4 ),

    HOEHE( "Geländehöhe", false, true, true, 4 ),

    RAUHEIT( "Rauheit", false, true, false, 4 ),

    RECHTSWERT( "Rechtswert", true, true, true, 4 ),

    FLIESSZONE( "Trennflächen", false, false, false, 4 ),

    DURCHSTROEMTE( "Durchströmte Bereiche", false, false, false, 4 ),

    BORDVOLL( "Bordvollpunkte", true, false, false, 4 );
    private POINT_PROPERTY( final String label, final boolean optional, final boolean visible,
        final boolean interpolation, final int precision )
    {
      m_parameters.put( PARAMETER.VISIBLE, visible );
      m_parameters.put( PARAMETER.OPTIONAL, optional );
      m_parameters.put( PARAMETER.LABEL, label );
      m_parameters.put( PARAMETER.INTERPOLATION, interpolation );
      m_parameters.put( PARAMETER.PRECISION, precision );
    }

    public Object setParameter( Object key, Object value )
    {
      return m_parameters.put( key, value );
    }

    public Object getParameter( Object key )
    {
      return m_parameters.get( key );
    }

    @Override
    public String toString( )
    {
      return m_parameters.get( PARAMETER.LABEL ).toString();
    }

    @SuppressWarnings("unchecked")
    private final HashMap<Object, Object> m_parameters = new HashMap();

  }
}
