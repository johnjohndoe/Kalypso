/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.mapeditor.views;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ogc.gml.FeatureTypeStyleTreeObject;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.RuleTreeObject;
import org.kalypso.ogc.gml.UserStyleTreeObject;
import org.kalypso.ui.editor.styleeditor.SLDEditorGuiBuilder;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.UserStyle;

public class StyleEditorViewPart extends ViewPart implements ISelectionChangedListener
{
  private ISelectionProvider m_gmop = null;

  private SLDEditorGuiBuilder m_guiBuilder = null;

  private FormToolkit m_formToolkit;

  public void setSelectionChangedProvider( final ISelectionProvider selectionProvider )
  {
    if( m_gmop != selectionProvider )
    {
      m_gmop = selectionProvider;
      m_gmop.addSelectionChangedListener( this );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_gmop != null )
      m_gmop.removeSelectionChangedListener( this );

    if( m_formToolkit != null )
      m_formToolkit.dispose();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_formToolkit = new FormToolkit( parent.getDisplay() );
    m_guiBuilder = new SLDEditorGuiBuilder( m_formToolkit, parent );
  }

  private void setStyle( final KalypsoUserStyle userStyle, final IKalypsoFeatureTheme theme, final int index )
  {
    m_guiBuilder.setStyle( userStyle, theme, index );
  }

  public void setStyle( final KalypsoUserStyle userStyle, final IKalypsoFeatureTheme theme )
  {
    m_guiBuilder.setStyle( userStyle, theme );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_guiBuilder != null )
      m_guiBuilder.setFocus();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    final Object o = ((IStructuredSelection) event.getSelection()).getFirstElement();
    if( o instanceof IKalypsoFeatureTheme )
    {
      // Reset style-editor, but the styles are not unique, so do not set anything
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) o;
      final UserStyle[] styles = theme.getStyles();
      if( styles != null && styles.length == 1 && styles[0] instanceof KalypsoUserStyle )
        setStyle( (KalypsoUserStyle) styles[0], theme );
      else
        setStyle( null, null );
    }
    else if( o instanceof IKalypsoTheme )
      setStyle( null, null );
    else if( o instanceof UserStyleTreeObject )
    {
      final IKalypsoTheme theme = ((UserStyleTreeObject) o).getParent();
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final KalypsoUserStyle kalypsoStyle = ((UserStyleTreeObject) o).getStyle();
        setStyle( kalypsoStyle, (IKalypsoFeatureTheme) theme );
      }
      else
        setStyle( null, null );
    }
    else if( o instanceof FeatureTypeStyleTreeObject )
    {
      final FeatureTypeStyleTreeObject ftsNode = (FeatureTypeStyleTreeObject) o;
      final UserStyleTreeObject userStyleNode = ftsNode.getParent();
      final KalypsoUserStyle userStyle = userStyleNode.getStyle();
      final IKalypsoTheme theme = userStyleNode.getParent();
      if( theme instanceof IKalypsoFeatureTheme )
      {
        setStyle( userStyle, (IKalypsoFeatureTheme) theme );
      }
      else
        setStyle( null, null );
    }
    else if( o instanceof RuleTreeObject )
    {
      final RuleTreeObject ruleNode = (RuleTreeObject) o;
      final Rule indexRule = ruleNode.getRule();
      final FeatureTypeStyleTreeObject ftsNode = ruleNode.getParent();
      final FeatureTypeStyle fts = ftsNode.getStyle();
      final Rule[] rules = fts.getRules();
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

      final UserStyleTreeObject userStyleNode = ftsNode.getParent();
      final KalypsoUserStyle userStyle = userStyleNode.getStyle();
      setStyle( userStyle, userStyleNode.getParent(), index );
    }
  }
}