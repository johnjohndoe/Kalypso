package org.kalypso.model.wspm.ui.editor;

import org.kalypso.model.wspm.core.profil.IProfil;

/**
 * @author gernot
 *
 */
public interface IProfilchartEditorListener
{
  /** Called, when the Profil-Reference of the ProfilchartEditor changes. */
  public void onProfilChanged( final ProfilchartEditor editor, final IProfil newprofil );
}
