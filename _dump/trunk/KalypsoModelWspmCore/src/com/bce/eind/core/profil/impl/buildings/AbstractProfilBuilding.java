/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilBuildingException;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;
import com.bce.eind.core.profil.impl.PlainProfil;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding implements IProfilBuilding
{
  private final BUILDING_TYP m_buildingTyp;

  private final Map<BUILDING_PROPERTY, Double> m_buildingValues = new LinkedHashMap<BUILDING_PROPERTY, Double>();

  private final PointProperty[] m_pointProperties;

  public AbstractProfilBuilding( final BUILDING_TYP buildingTyp,
      final Collection<BUILDING_PROPERTY> properties, final PointProperty[] pointProperties )
  {
    m_buildingTyp = buildingTyp;
    m_pointProperties = pointProperties == null ? new PointProperty[] {} : pointProperties;

    for( final BUILDING_PROPERTY property : properties )
      m_buildingValues.put( property, new Double( 0.0 ) );
  }

  @SuppressWarnings("unused")
  public  void addProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final PointProperty property : getProfilPointProperties() )
      profil.addPointProperty( property );
  }
  @SuppressWarnings("unused")
  public void removeProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final PointProperty property : getProfilPointProperties() )
      profil.removePointProperty( property );
  }
  

  /**
   * @return Returns the buildingTyp.
   */
  public BUILDING_TYP getBuildingTyp( )
  {
    return m_buildingTyp;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getTableDataKeys()
   */
  public PointProperty[] getProfilPointProperties( )
  {
    return m_pointProperties;
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
   * @see com.bce.eind.core.profil.IProfilBuilding#getProfilBuildingProperties()
   */
  public Collection<BUILDING_PROPERTY> getProfilBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
