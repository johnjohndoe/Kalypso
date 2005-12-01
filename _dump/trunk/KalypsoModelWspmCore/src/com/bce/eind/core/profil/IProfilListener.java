package com.bce.eind.core.profil;

import com.bce.eind.core.profil.changes.AbstractChange;

/**
 * @author Belger
 */
public interface IProfilListener
{
  /** Wird aufgerufen, wenn sich ein oder mehrere Werte eines Punktes ge�ndert haben */
  public void onPointsChanged( final AbstractChange change );

  public void onPointsAdded( final AbstractChange change );

  public void onPointsRemoved( final AbstractChange change );

  public void onPointPropertiesAdded( final AbstractChange change );

  public void onPointPropertiesRemoved( final AbstractChange change );

  /** Wird nur aufgerufen, wenn sich die Eigenschaft Building_typ �ndert */
  public void onBuildingAdded(AbstractChange change  );

  /** Wird aufgerufen, wenn sich ein oder mehrere Eigenschaften Geb�udes ge�ndert haben */
  public void onBuildingChanged( AbstractChange change );
  public void onBuildingRemoved( AbstractChange change );
  /**
   * Wird aufgerufen, wenn sich ein oder mehrere Profil_Properties ge�ndert haben see
   * IProfil.PROFIL_PROPERTY
   */
  public void onProfilDataChanged(AbstractChange change  );

  /**
   * @param change
   *          wenn != Null wurde der Trenner verschoben
   */
  public void onDeviderChanged( final AbstractChange change );

  /** Wird aufgerufen, wenn ein oder mehrere Trenner gel�scht wurden */
  public void onDeviderRemoved( final AbstractChange change );

  public void onDeviderAdded( final AbstractChange change );
}