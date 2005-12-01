package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_TYP;
import com.bce.eind.core.profil.impl.PlainProfil;

public class BuildingAdd extends AbstractChange
{

  public BuildingAdd(final BUILDING_TYP newBuildingTyp )
  {
    super( null, null, newBuildingTyp);
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange( PlainProfil profil ) throws ProfilDataException
  {
    m_oldValue = profil.getBuilding();
    profil.removeBuilding();
    profil.setBuilding( (BUILDING_TYP)m_newValue );
  }

  /** 
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener,
   *      com.bce.eind.core.profil.changes.AbstractChange[])
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
    listener.onBuildingAdded( this );

  }

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final BUILDING_TYP bt = (BUILDING_TYP)m_oldValue;
    return new BuildingAdd( bt );
  }

}
