/**
 * 
 */
package xp;

import org.kalypso.ui.editor.mapeditor.actiondelegates.AbstractGisMapEditorActionDelegate;

/**
 * @author Gernot Belger
 */
public class CreateGridDelegate extends AbstractGisMapEditorActionDelegate
{
  public CreateGridDelegate( )
  {
    super( new CreateGitterWidget() );
  }
}
