/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;

import de.belger.swtchart.layer.IChartLayer;

/**
 * @author gernot
 * 
 */
public interface IProfilChartLayer extends IChartLayer
{
  /** Erzeugt eine Profil-View, welche die Spezifika dieses Layers anzeigt. */
  public IProfilView createLayerPanel( final IProfilEventManager pem,
      final ProfilViewData viewData );

  /**
   * Löscht diesen Layer aus dem Profil. Besser gesagt, löscht die Daten aus dem
   * Profil, die durch diesen Layer repräsentiert werden.
   * @throws ProfilDataException 
   * 
   * @throws UnsupportedOperationException Falls diese Art von Layer nicht gelöscht werden kann.
   */
  public void removeYourself( ) throws ProfilDataException;

}
