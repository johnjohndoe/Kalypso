/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import javax.xml.namespace.QName;

import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateTransitionElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public class CreateTransitionElementWidget extends AbstractDelegateWidget
{
  private IKalypsoFeatureTheme m_mapActiveTheme;

  private IFEDiscretisationModel1d2d m_discretisationModel;

  private IContinuityLine1D m_line1D;

  private IContinuityLine2D m_line2D;

  private ITransitionElement.TRANSITION_TYPE m_transition_type;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final SelectFeatureWidget m_selDelegateWidget;

  public CreateTransitionElementWidget( )
  {
    super( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateTransitionElementWidget.0"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateTransitionElementWidget.1"), new SelectFeatureWidget( "", "", new QName[] { IContinuityLine1D.QNAME, IContinuityLine2D.QNAME }, IFELine.PROP_GEOMETRY ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateTransitionElementWidget.4") ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );

    m_selDelegateWidget = (SelectFeatureWidget) getDelegate();

    m_transition_type = ITransitionElement.TRANSITION_TYPE.TYPE1D2D;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateTransitionElementWidget.5") + delegateTooltip ); //$NON-NLS-1$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private void reinit( )
  {
    m_line1D = null;
    m_line2D = null;

    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();
    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
      m_mapActiveTheme = (IKalypsoFeatureTheme) activeTheme;
    else
      return;
    m_discretisationModel = UtilMap.findFEModelTheme( mapPanel );
    if( m_discretisationModel == null )
      return;

    final IKalypsoTheme[] themes = mapModell.getAllThemes();
    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final QName qName = ft.getFeatureType().getQName();
        if( qName.equals( IFELine.QNAME ) )
        {
          final IKalypsoFeatureTheme[] fts = new IKalypsoFeatureTheme[1];
          fts[0] = ft;
          m_selDelegateWidget.setThemes( fts );
        }
      }
    }
    final FeatureList featureList = m_mapActiveTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_discretisationModel = (IFEDiscretisationModel1d2d) parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();
    mapPanel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyChar() == KeyEvent.VK_ESCAPE )
      reinit();

    else if( e.getKeyChar() == KeyEvent.VK_ENTER )
    {
      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
      final EasyFeatureWrapper[] eFeatures = selectionManager.getAllFeatures();
      // If ENTER is pressed and no features are selected, java.lang.ArrayIndexOutOfBoundsException occurs.
      // We will just ignore ENTER if no features are selected.
      if( eFeatures.length == 0 )
      {
        super.keyPressed( e );
        return;
      }
      final CommandableWorkspace workspace = eFeatures[0].getWorkspace();
      final Feature[] features = FeatureSelectionHelper.getFeatures( selectionManager );
      for( final Feature feature : features )
      {
        final Object object2d = feature.getAdapter( IContinuityLine2D.class );

        if( object2d != null )
          m_line2D = (IContinuityLine2D) object2d;

        final Object object1d = feature.getAdapter( IContinuityLine1D.class );

        if( object1d != null )
          m_line1D = (IContinuityLine1D) object1d;

        if( m_line1D != null && m_line2D != null )
          break;
      }

      if( m_line1D != null && m_line2D != null )
      {
        final Display display = PlatformUI.getWorkbench().getDisplay();

        /* Ask user for type of new feature */
        final TransitionElementTypeSelectionWizard wizard = new TransitionElementTypeSelectionWizard();

        display.asyncExec( new Runnable()
        {
          @Override
          @SuppressWarnings("synthetic-access")
          public void run( )
          {
            final Shell shell = display.getActiveShell();
            final WizardDialog dialog = new WizardDialog( shell, wizard );
            if( dialog.open() == Window.CANCEL )
              return;
            m_transition_type = wizard.getSelectedType();
            final CreateTransitionElementCommand command = new CreateTransitionElementCommand( m_discretisationModel, m_line1D, m_line2D, m_transition_type );
            try
            {
              workspace.postCommand( command );
              getMapPanel().getSelectionManager().clear();
            }
            catch( final Exception e1 )
            {
              e1.printStackTrace();
            }
            finally
            {
              reinit();
            }
          }
        } );

      }
    }
    super.keyPressed( e );
  }

  @Override
  public void finish( )
  {
    /* Deselect all */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();
    super.finish();
  }
}