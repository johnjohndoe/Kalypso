package org.kalypso.model.wspm.ui.view;

import org.eclipse.ui.IViewPart;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;


/**
 * Marker interface for Views linked to {@link com.bce.profil.eclipse.editor.ProfilchartEditor}
 * 
 * @author belger
 */
public interface IProfilViewPart extends IViewPart
{
  public void setProfilchartEditor( final ProfilchartEditor editor );
  
  public ProfilchartEditor getProfilchartEditor();
}
