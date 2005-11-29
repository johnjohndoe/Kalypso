package com.bce.eind.core.profil;

import com.bce.eind.core.profil.changes.BuildingChange;
import com.bce.eind.core.profil.changes.DeviderChange;
import com.bce.eind.core.profil.changes.PointChange;
import com.bce.eind.core.profil.changes.ProfilChange;

/**
 * @author Belger
 */
public interface IProfilListener
{
  /** Wird aufgerufen, wenn sich ein oder mehrere Werte eines Punktes geändert haben */
  public void onPointsChanged( final PointChange[] changes );

  public void onPointsAdded( final PointChange[] changes );

  public void onPointsRemoved( final PointChange[] changes );

  public void onPointPropertiesAdded( final PointChange[] changes );

  public void onPointPropertiesRemoved( final PointChange[] changes );

  /** Wird nur aufgerufen, wenn sich die Eigenschaft Building_typ ändert */
  public void onBuildingAdded(BuildingChange[] changes  );

  /** Wird aufgerufen, wenn sich ein oder mehrere Eigenschaften Gebäudes geändert haben */
  public void onBuildingChanged( BuildingChange[] changes );
  public void onBuildingRemoved( BuildingChange[] changes );
  /**
   * Wird aufgerufen, wenn sich ein oder mehrere Profil_Properties geändert haben see
   * IProfil.PROFIL_PROPERTY
   */
  public void onProfilDataChanged(ProfilChange[] changes  );

  /**
   * @param changes
   *          wenn != Null wurde der Trenner verschoben
   */
  public void onDeviderChanged( final DeviderChange[] changes );

  /** Wird aufgerufen, wenn ein oder mehrere Trenner gelöscht wurden */
  public void onDeviderRemoved( final DeviderChange[] changes );

  public void onDeviderAdded( final DeviderChange[] changes );
}