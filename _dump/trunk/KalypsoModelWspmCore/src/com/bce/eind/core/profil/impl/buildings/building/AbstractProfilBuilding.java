/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings.building;

import java.util.Collection;
import java.util.Collections;

import com.bce.eind.core.profil.ProfilBuildingException;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;
import com.bce.eind.core.profil.impl.buildings.AbstractBuilding;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding extends AbstractBuilding
{
  
  private final POINT_PROPERTY[] m_pointProperties;

  public AbstractProfilBuilding( final BUILDING_TYP buildingTyp,
      final Collection<BUILDING_PROPERTY> properties, final POINT_PROPERTY[] pointProperties )
  {
    super (buildingTyp,properties);

    m_pointProperties = pointProperties == null ? new POINT_PROPERTY[] {} : pointProperties;

  }

  @SuppressWarnings("unused")
  public  void addProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final POINT_PROPERTY property : getPointProperties() )
      profil.addPointProperty( property );
  }
  @SuppressWarnings("unused")
  public void removeProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final POINT_PROPERTY property : getPointProperties() )
      profil.removePointProperty( property );
  }
  

  /**
   * @return Returns the buildingTyp.
   */
  public BUILDING_TYP getTyp( )
  {
    return m_buildingTyp;
  }

  /**
   * @see com.bce.eind.core.profilinterface.IProfilBuilding#getTableDataKeys()
   */
  public POINT_PROPERTY[] getPointProperties( )
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
   * @see com.bce.eind.core.profil.IProfilBuilding#getBuildingProperties()
   */
  public Collection<BUILDING_PROPERTY> getBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
