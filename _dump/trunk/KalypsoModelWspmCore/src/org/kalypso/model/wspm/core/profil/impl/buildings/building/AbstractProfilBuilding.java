/*
 * Created on 31.03.2005
 */
package org.kalypso.model.wspm.core.profil.impl.buildings.building;

import java.util.Collection;
import java.util.Collections;

import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.PlainProfil;
import org.kalypso.model.wspm.core.profil.impl.buildings.AbstractBuilding;


/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding extends AbstractBuilding
{

  private final POINT_PROPERTY[] m_pointProperties;

  public AbstractProfilBuilding( final BUILDING_TYP buildingTyp,
      final Collection<BUILDING_PROPERTY> properties, final POINT_PROPERTY[] pointProperties )
  {
    super( buildingTyp, properties );

    m_pointProperties = pointProperties == null ? new POINT_PROPERTY[] {} : pointProperties;

  }

  public void addProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final POINT_PROPERTY property : getPointProperties() )
      profil.addPointProperty( property );
  }

  public void removeProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final POINT_PROPERTY property : getPointProperties() )
      profil.removePointProperty( property );
  }

  /**
   * @return Returns the buildingTyp.
   */
  @Override
  public BUILDING_TYP getTyp( )
  {
    return m_buildingTyp;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilBuilding#getTableDataKeys()
   */
  public POINT_PROPERTY[] getPointProperties( )
  {
    return m_pointProperties;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilBuilding#hasProperty(org.kalypso.model.wspm.core.profil.BUILDING_PROPERTY)
   */
  @Override
  public boolean hasProperty( BUILDING_PROPERTY profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilBuilding#getBuildingProperties()
   */
  @Override
  public Collection<BUILDING_PROPERTY> getBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
