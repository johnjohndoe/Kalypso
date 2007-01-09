/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.ui.map;

import org.kalypso.ui.editor.mapeditor.actiondelegates.AbstractGisMapEditorActionDelegate;

/**
 * @author Gernot Belger
 */
public class CreateFEContlineDelegate extends AbstractGisMapEditorActionDelegate
{
  public CreateFEContlineDelegate( )
  {
    super( new CreateFEContlineWidget() );
  }
}
