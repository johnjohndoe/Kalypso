package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

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
    hint.setBuildingChanged();
    hint.setPointPropertiesChanged();
    
    final IProfilBuilding oldBuilding = m_profil.getBuilding();
    m_profil.setBuilding( m_building );

    return new BuildingSet( m_profil, oldBuilding );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
    // TODO Auto-generated method stub
    return null;
  }

 
}
