/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import org.deegree.graphics.sld.Stroke;
import org.eclipse.swt.widgets.Composite;

/**
 * @author F.Lindemann
 *  
 */
public class StrokeLinejoinComboPanel extends ComboPanel
{

  public StrokeLinejoinComboPanel( Composite parent, String label, int value )
  {
    super( parent, label );
    items = new String[3];
    items[0] = "mitre";
    items[1] = "round";
    items[2] = "bevel";
    init();
    setSelection( value );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#getSelection()
   */
  public int getSelection()
  {
    switch( selection_index )
    {
    case 0:
      return Stroke.LJ_MITRE;
    case 1:
      return Stroke.LJ_ROUND;
    case 2:
      return Stroke.LJ_BEVEL;
    default:
      return Stroke.LJ_DEFAULT;
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
   */
  public void setSelection( int index )
  {
    switch( index )
    {
    case Stroke.LJ_MITRE:
      comboBox.select( 0 );
      break;
    case Stroke.LJ_ROUND:
      comboBox.select( 1 );
      break;
    case Stroke.LJ_BEVEL:
      comboBox.select( 2 );
      break;
    default:
      comboBox.select( 0 );
    }
  }
}