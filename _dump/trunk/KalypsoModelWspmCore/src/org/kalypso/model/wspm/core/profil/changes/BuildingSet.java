package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

public class BuildingSet implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfilBuilding m_building;
/**
 * 
 * @param profil
 * @param building maybe null to remove building
 */
  public BuildingSet( final IProfil profil, final IProfilBuilding building )
  {
    m_profil = profil;
    m_building = building;
  }

  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    if (hint!=null) hint.setBuildingChanged();
    if (hint!=null) hint.setPointPropertiesChanged();
    
    final IProfilBuilding oldBuilding = m_profil.getBuilding();
    m_profil.setBuilding( m_building );

    return new BuildingSet( m_profil, oldBuilding );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_building;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
    
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
    
    return null;
  }

 
}
