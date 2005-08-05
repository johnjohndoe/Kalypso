/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil;

import java.util.HashMap;

/**
 * @author kimwerner
 */
public class PointProperty
{
  

  @SuppressWarnings( "unchecked" )
  private final HashMap<Object, Object> m_parameters = new HashMap();

  public enum PARAMETER
  {
    LABEL, OPTIONAL, VISIBLE, PRECISION, INTERPOLATION
  };

  public final static PointProperty BEWUCHS_AX = new PointProperty( "AX", true, true, false, 4 );

  public final static PointProperty BEWUCHS_AY = new PointProperty( "AY", true, true, false, 4);

  public final static PointProperty BEWUCHS_DP = new PointProperty( "DP", true, true, false, 4 );

  public final static PointProperty BREITE = new PointProperty( "Breite", false, true, true, 4 );

 // public final static PointProperty DURCHSTROEMTE = new PointProperty( "Durchströmte Bereiche",
 //     false, false, false, 4 );

  public final static PointProperty HOCHWERT = new PointProperty( "Hochwert", true, true, true, 4 );

  public final static PointProperty UNTERKANTEBRUECKE = new PointProperty( "Brückenunterkante",
      true, true, false, 4 );

  public final static PointProperty OBERKANTEBRUECKE = new PointProperty( "Brückenoberkante", true,
      true, false, 4 );

  public final static PointProperty OBERKANTEWEHR = new PointProperty( "Wehr", true, true, false, 4 );

  public final static PointProperty HOEHE = new PointProperty( "Geländehöhe", false, true, true, 4 );

  public final static PointProperty RAUHEIT = new PointProperty( "Rauheit", false, true, false, 4 );

  public final static PointProperty RECHTSWERT = new PointProperty( "Rechtswert", true, true, true,
      4 );

 // public final static PointProperty TRENNFLAECHE = new PointProperty( "Trennflächen", false, false,
 //     false, 4 );

 // public final static PointProperty BORDVOLL = new PointProperty( "Bordvollpunkte", true, false,
 //     false, 4 );

   
  public PointProperty( final String label, final boolean optional, final boolean visible,
      final boolean interpolation, final int precision)
  {
    m_parameters.put( PARAMETER.VISIBLE, visible );
    m_parameters.put( PARAMETER.OPTIONAL, optional );
    m_parameters.put( PARAMETER.LABEL, label );
    m_parameters.put( PARAMETER.INTERPOLATION, interpolation );
    m_parameters.put( PARAMETER.PRECISION, precision );
 
  }
public Object setParameter(Object key,Object value)
{
  return m_parameters.put(key,value);
}
public Object getParameter(Object key)
{
  return m_parameters.get(key);
}
  public PointProperty( )
  {
    this( "", true, false, false, 4 );
  }


  @Override
  public String toString( )
  {
    return m_parameters.get( PARAMETER.LABEL ).toString();
  }
}
