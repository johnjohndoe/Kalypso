/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.ProfilBuildingException;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.PointProperty;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding implements IProfilBuilding
{
  private final IProfil.BUILDING_TYP m_buildingTyp;

  private final Map<ProfilBuildingProperty, Double> m_buildingValues = new LinkedHashMap<ProfilBuildingProperty, Double>();

  private final PointProperty[] m_pointProperties;

  public AbstractProfilBuilding( final IProfil.BUILDING_TYP buildingTyp, final Collection<ProfilBuildingProperty> properties, final PointProperty[] pointProperties )
  {
    m_buildingTyp = buildingTyp;
    m_pointProperties = pointProperties == null ? new PointProperty[] {} : pointProperties;
    
    for( final ProfilBuildingProperty property : properties )
      m_buildingValues.put( property, new Double( 0.0 ) );
  }

  /**
   * @return Returns the buildingTyp.
   */
  public IProfil.BUILDING_TYP getBuildingTyp( )
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
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getValue(com.bce.eind.core.profilinterface.IProfil.ProfilBuildingProperty)
   */
  public double getValue( ProfilBuildingProperty buildingValue ) throws ProfilBuildingException
  {
    if( m_buildingValues.containsKey( buildingValue ) )
      return m_buildingValues.get( buildingValue );
    throw new ProfilBuildingException( "Eigenschaft existiert nicht" );
  }

  /**
   * @return 
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#setValue(com.bce.eind.core.profilinterface.IProfil.ProfilBuildingProperty,
   *      double)
   */
  public boolean setValue( final ProfilBuildingProperty property, final double value )
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
   * @see com.bce.eind.core.profil.IProfilBuilding#hasProperty(com.bce.eind.core.profil.ProfilBuildingProperty)
   */
  public boolean hasProperty( ProfilBuildingProperty profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilBuilding#getProfilBuildingProperties()
   */
  public Collection<ProfilBuildingProperty> getProfilBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
