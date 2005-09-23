/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.ProfilBuildingException;

/**
 * @author kimwerner
 */
public abstract class AbstractBuilding implements IProfilBuilding
{
  protected final BUILDING_TYP m_buildingTyp;

  protected final Map<BUILDING_PROPERTY, Double> m_buildingValues = new LinkedHashMap<BUILDING_PROPERTY, Double>();

  public AbstractBuilding( final BUILDING_TYP buildingTyp,
      final Collection<BUILDING_PROPERTY> properties )
  {
    m_buildingTyp = buildingTyp;

    for( final BUILDING_PROPERTY property : properties )
      m_buildingValues.put( property, new Double( 0.0 ) );
  }

  /**
   * @return Returns the buildingTyp.
   */
  public BUILDING_TYP getTyp( )
  {
    return m_buildingTyp;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getValue(com.bce.eind.core.profilinterface.IProfil.BUILDING_PROPERTY,
   *      TYPE)
   */
  public double getValue( BUILDING_PROPERTY buildingValue ) throws ProfilBuildingException
  {
    if( m_buildingValues.containsKey( buildingValue ) )
      return m_buildingValues.get( buildingValue );
    throw new ProfilBuildingException( "Eigenschaft existiert nicht" );
  }

  /**
   * @return
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#setValue(com.bce.eind.core.profilinterface.IProfil.BUILDING_PROPERTY,
   *      double)
   */
  public boolean setValue( final BUILDING_PROPERTY property, final double value )
      throws ProfilBuildingException
  {
    if( !m_buildingValues.containsKey( property ) )
      throw new ProfilBuildingException( "ungültige Eigenschaft für dieses Gebäude" );

    final Double oldValue = m_buildingValues.get( property );
    if( oldValue.compareTo( value ) != 0 )
    {
      m_buildingValues.put( property, value );
      return true;
    }

    return false;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilBuilding#hasProperty(com.bce.eind.core.profil.BUILDING_PROPERTY)
   */
  public boolean hasProperty( BUILDING_PROPERTY profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilBuilding#getBuildingProperties()
   */
  public Collection<BUILDING_PROPERTY> getBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
