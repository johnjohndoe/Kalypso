package org.kalypso.ui.editor.mapeditor.views;

import org.deegree.graphics.sld.Rule;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.outline.RuleTreeObject;
import org.kalypso.ogc.gml.outline.ThemeStyleTreeObject;
import org.kalypso.ui.editor.styleeditor.SLDEditorGuiBuilder;

/**
 * 
 *  
 */

public class StyleEditorViewPart extends ViewPart implements ISelectionChangedListener
{
  private ISelectionProvider gmop = null;

  private SLDEditorGuiBuilder guiBuilder = null;

  public void setSelectionChangedProvider( final ISelectionProvider selectionProvider )
  {
    if( this.gmop != selectionProvider )
    {
      this.gmop = selectionProvider;
      gmop.addSelectionChangedListener( this );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    super.dispose();
    if( gmop != null )
      gmop.removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    guiBuilder = new SLDEditorGuiBuilder( parent);
  }

  public void initStyleEditor( KalypsoUserStyle userStyle, IKalypsoFeatureTheme theme, int index )
  {
    guiBuilder.buildSWTGui( userStyle, theme, index );
  }

  public void initStyleEditor( KalypsoUserStyle userStyle, IKalypsoFeatureTheme theme )
  {
  	guiBuilder.buildSWTGui( userStyle, theme);
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // 
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    Object o = ( (IStructuredSelection)event.getSelection() ).getFirstElement();
    if( o instanceof ThemeStyleTreeObject )
    {
      final IKalypsoTheme theme = ( (ThemeStyleTreeObject)o ).getTheme();
      if( !( theme instanceof IKalypsoFeatureTheme ) )
        initStyleEditor( null, null );
      else
      {        
        KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
        initStyleEditor( kalypsoStyle, (IKalypsoFeatureTheme)theme );
      }
    }
    else if( o instanceof IKalypsoTheme )
      initStyleEditor( null, null );
    else if( o instanceof RuleTreeObject )
    {
      RuleTreeObject obj = (RuleTreeObject)o;
      Rule indexRule = obj.getRule();
      Rule[] rules = obj.getStyle().getFeatureTypeStyles()[0].getRules();
      int index = -1;
      if( indexRule != null )
      {
        for( int i = 0; i < rules.length; i++ )
        {
          if( rules[i] == indexRule )
          {
            index = i;
            break;
          }
        }
      }
      initStyleEditor( obj.getStyle(), obj.getTheme(), index );
    }
  }
}