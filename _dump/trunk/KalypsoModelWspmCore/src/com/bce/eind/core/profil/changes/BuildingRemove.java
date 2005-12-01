package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.PlainProfil;

public class BuildingRemove extends AbstractChange
{

  
  public BuildingRemove()
  {
    super( null, null, null );
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange(PlainProfil profil ) throws ProfilDataException
  {
    m_oldValue = profil.removeBuilding();
  }
  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener, com.bce.eind.core.profil.changes.AbstractChange[])
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
   listener.onBuildingRemoved(this);
    
  }

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final IProfilBuilding b = (IProfilBuilding)m_oldValue;
    return new BuildingAdd(b.getTyp() );
  }

}
