/**
 * 
 */
package org.kalypso.model.flood.ui.map;

import org.kalypso.ui.editor.mapeditor.actiondelegates.AbstractGisMapEditorActionDelegate;

/**
 * @author Gernot Belger
 */
public class CreateFeatureDelegate extends AbstractGisMapEditorActionDelegate
{
  public CreateFeatureDelegate( )
  {
    super( new CreateFeatureWidget() );
  }
}
