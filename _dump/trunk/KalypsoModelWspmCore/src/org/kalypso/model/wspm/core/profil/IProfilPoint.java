/*
 * Created on 22.02.2005
 */
package org.kalypso.model.wspm.core.profil;

import java.util.Collection;
import java.util.HashMap;

/**
 * Kim: Document the interface methods
 * 
 * @author kimwerner
 */
public interface IProfilPoint
{
  public double getValueFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException;

  public boolean setValueFor( final POINT_PROPERTY pointProperty, final double value );

  public boolean hasProperty( final POINT_PROPERTY pointProperty );

  public IProfilPoint clonePoint( );

  public Collection<POINT_PROPERTY> getProperties( );

  public static enum PARAMETER
  {
    LABEL,
    OPTIONAL,
    VISIBLE,
    PRECISION,
    INTERPOLATION
  };

  public static enum POINT_PROPERTY
  {
    BEWUCHS_AX("AX", true, true, false, 0.0001),

    BEWUCHS_AY("AY", true, true, false, 0.0001),

    BEWUCHS_DP("DP", true, true, false, 0.0001),

    BREITE("Breite", false, true, true, 0.0001),

    HOCHWERT("Hochwert", true, true, true, 0.0001),

    UNTERKANTEBRUECKE("Brückenunterkante", true, true, true, 0.0001),

    OBERKANTEBRUECKE("Brückenoberkante", true, true, true, 0.0001),

    OBERKANTEWEHR("Wehr", true, true, true, 0.0001),

    HOEHE("Geländehöhe", false, true, true, 0.0001),

    RAUHEIT("Rauheit", false, true, false, 0.0001),

    RECHTSWERT("Rechtswert", true, true, true, 0.0001),

    FLIESSZONE("Trennflächen", false, false, false, 0.0001),

    DURCHSTROEMTE("Durchströmte Bereiche", false, false, false, 0.0001),

    BORDVOLL("Bordvollpunkte", true, false, false, 0.0001);
    @SuppressWarnings("boxing")
    private POINT_PROPERTY( final String label, final boolean optional, final boolean visible, final boolean interpolation, final double precision )
    {
      m_parameters.put( PARAMETER.VISIBLE, visible );
      m_parameters.put( PARAMETER.OPTIONAL, optional );
      m_parameters.put( PARAMETER.LABEL, label );
      m_parameters.put( PARAMETER.INTERPOLATION, interpolation );
      m_parameters.put( PARAMETER.PRECISION, precision );
    }

    public Object getParameter( Object key )
    {
      return m_parameters.get( key );
    }

    @SuppressWarnings("boxing")
    public void setVisible( final boolean value )
    {
      m_parameters.put( PARAMETER.VISIBLE, value );
    }

    @Override
    public String toString( )
    {
      return m_parameters.get( PARAMETER.LABEL ).toString();
    }

    public POINT_PROPERTY[] getDependencies( )
    {
      return null;
    }

    @SuppressWarnings("unchecked")
    private final HashMap<Object, Object> m_parameters = new HashMap();

  }
}
