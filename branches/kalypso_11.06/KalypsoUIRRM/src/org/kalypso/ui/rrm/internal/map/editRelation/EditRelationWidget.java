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
package org.kalypso.ui.rrm.internal.map.editRelation;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.gmleditor.command.AddHeavyRelationshipCommand;
import org.kalypso.ui.editor.gmleditor.command.AddRelationCommand;
import org.kalypso.ui.editor.gmleditor.command.RemoveHeavyRelationCommand;
import org.kalypso.ui.editor.gmleditor.command.RemoveRelationCommand;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypso.ui.rrm.internal.map.editRelation.EditRelationData.MODE;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.CascadingFeatureList;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Widget where the user can create relations between selected features. only features from the workspace of the active
 * featuretheme can be selected <br>
 * Constraints from gml-application schemas are supported. TODO use check icons that indicate mixed child status <br>
 * TODO support removing relations <br>
 * 
 * @author doemming
 */
public class EditRelationWidget extends AbstractWidget implements IWidgetWithOptions
{
  private static final double RADIUS = 30;

  private final EditRelationData m_data = new EditRelationData();

  private FeatureList m_allowedFeatureList = null;

  final StringBuffer m_fitProblems = new StringBuffer();

  private EditRelationViewer m_editRelationViewer;

  public EditRelationWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * empty constructor so widget can be used with SelectWidgetHandler
   */
  public EditRelationWidget( )
  {
    super( Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.2" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void leftPressed( final Point p )
  {
    final Feature sourceFeature = m_data.getSourceFeature();
    final Feature targetFeature = m_data.getTargetFeature();

    if( sourceFeature != null && targetFeature != null )
    {
      perform();
      finish();
      return;
    }

    final IMapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final GM_Point point = GeometryFactory.createGM_Point( p, transform, mapPanel.getMapModell().getCoordinatesSystem() );

    final double r = transform.getSourceX( RADIUS ) - transform.getSourceX( 0 );

    final Feature newSource = (Feature) JMSelector.selectNearest( point, r, m_allowedFeatureList, false );
    m_data.setFeatures( newSource, null );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    super.finish();

    m_data.setFeatures( null, null );
  }

  @Override
  public void dragged( final Point p )
  {
    moved( p );

    repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    super.moved( p );

    final Feature sourceFeature = m_data.getSourceFeature();
    if( sourceFeature == null )
      return;

    final IMapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final GM_Point point = GeometryFactory.createGM_Point( p, transform, mapPanel.getMapModell().getCoordinatesSystem() );
    final double r = transform.getSourceX( RADIUS ) - transform.getSourceX( 0 );

    final Feature newTarget = (Feature) JMSelector.selectNearest( point, r, m_allowedFeatureList, false );

    m_data.setFeatures( sourceFeature, newTarget );

    repaintMap();
  }

  @Override
  public void leftReleased( final Point p )
  {
    perform();
    finish();
  }

  @Override
  public void paint( final Graphics g )
  {
    final Feature sourceFeature = m_data.getSourceFeature();
    final Feature targetFeature = m_data.getTargetFeature();

    if( sourceFeature == null || targetFeature == null )
      return;

    final GM_Object fromGeom = sourceFeature.getDefaultGeometryProperty();
    final GM_Object toGeom = targetFeature.getDefaultGeometryProperty();
    if( fromGeom == null || toGeom == null )
      return;

    final GM_Point fromCenter = fromGeom.getCentroid();
    final GM_Point toCenter = toGeom.getCentroid();
    final IMapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final int x1 = (int) transform.getDestX( fromCenter.getX() );
    final int y1 = (int) transform.getDestY( fromCenter.getY() );
    final int x2 = (int) transform.getDestX( toCenter.getX() );
    final int y2 = (int) transform.getDestY( toCenter.getY() );
    g.setColor( new Color( 1212121212 ) );
    g.drawLine( x1, y1, x2, y2 );
  }

  private FeatureList getAllowedFeatureList( )
  {
    final List<FeatureList> result = new ArrayList<FeatureList>();
    final IKalypsoTheme activeTheme = getActiveTheme();
    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel == null ? null : mapPanel.getMapModell();
    if( mapModell == null || activeTheme == null || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return new CascadingFeatureList( result.toArray( new FeatureList[result.size()] ) );

    final IKalypsoFeatureTheme activeFeatureTheme = (IKalypsoFeatureTheme) activeTheme;
    final GMLWorkspace workspace = activeFeatureTheme.getWorkspace();
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    for( final IKalypsoTheme element : allThemes )
    {
      if( element != null && element instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme kalypsoFeatureTheme = (IKalypsoFeatureTheme) element;
        if( kalypsoFeatureTheme.getWorkspace() == workspace )
          result.add( (kalypsoFeatureTheme).getFeatureList() );
      }
    }
    return new CascadingFeatureList( result.toArray( new FeatureList[result.size()] ) );
  }

  private void refreshSettings( )
  {
    m_allowedFeatureList = getAllowedFeatureList();
    final IKalypsoTheme activeTheme = getActiveTheme();
    final EditRelationInput input = buildInput( activeTheme );
    m_data.setInput( input );
  }

  private EditRelationInput buildInput( final IKalypsoTheme activeTheme )
  {
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) activeTheme;
      final CommandableWorkspace workspace = featureTheme.getWorkspace();
      if( workspace == null )
        return null;

      return new EditRelationInput( workspace );
    }

    return null;
  }

  public synchronized void perform( )
  {
    final Feature srcFeature = m_data.getSourceFeature();
    final Feature targetFeature = m_data.getTargetFeature();
    if( srcFeature == null || targetFeature == null )
      return;

    final List<IEditRelationType> fitList = m_data.getFitList( new StringBuilder() );

    m_editRelationViewer.getDisplay().asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        for( final IEditRelationType element : fitList )
        {
          System.out.println( element.toString() );
        }
        // TODO handle fitList.size()>1 with dialog
        if( fitList.size() < 1 )
          return;
        final IEditRelationType relation;
        if( fitList.size() == 1 )
        {
          relation = fitList.get( 0 );
        }
        else
        {
          final IStructuredContentProvider cProvider = new IStructuredContentProvider()
          {
            @Override
            public Object[] getElements( final Object inputElement )
            {
              if( inputElement instanceof List )
              {
                return ((List< ? >) inputElement).toArray();
              }
              return null;
            }

            @Override
            public void dispose( )
            {
              // m_input = null;
            }

            @Override
            public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
            {
              // m_input = newInput;
            }
          };

          final String msg = Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.6", fitList.size() ); //$NON-NLS-1$
          final ListSelectionDialog dialog = new ListSelectionDialog( m_editRelationViewer.getShell(), fitList, cProvider, new EditRelationOptionsLabelProvider(), msg );
          dialog.setInitialSelections( new Object[] { fitList.get( 0 ) } );
          dialog.setBlockOnOpen( true );
          boolean correct = false;
          Object[] result = null;
          while( !correct )
          {
            final int answer = dialog.open();
            if( answer == Window.CANCEL )
              return;
            result = dialog.getResult();
            correct = result.length == 1;
          }
          relation = (IEditRelationType) result[0];
        }

        final CommandableWorkspace workspace = ((IKalypsoFeatureTheme) getActiveTheme()).getWorkspace();
        final ICommand command;
        final MODE mode = m_data.getModificationMode();
        switch( mode )
        {
          case ADD:
            if( relation instanceof HeavyRelationType )
            {
              final HeavyRelationType heavyRealtion = (HeavyRelationType) relation;

              command = new AddHeavyRelationshipCommand( workspace, srcFeature, heavyRealtion.getLink1(), heavyRealtion.getBodyFT(), heavyRealtion.getLink2(), targetFeature );
            }
            else
            {
              final LightRelationType normalRelation = (LightRelationType) relation;
              command = new AddRelationCommand( srcFeature, normalRelation.getLink(), 0, targetFeature );
            }
            break;
          case REMOVE:
            if( relation instanceof HeavyRelationType )
            {
              final HeavyRelationType heavyRealtion = (HeavyRelationType) relation;

              final FindExistingHeavyRelationsFeatureVisitor visitor = new FindExistingHeavyRelationsFeatureVisitor( workspace, heavyRealtion );
              visitor.visit( srcFeature );
              final Feature[] bodyFeatureFor = visitor.getBodyFeatureFor( targetFeature );
              if( bodyFeatureFor.length > 0 )
                command = new RemoveHeavyRelationCommand( workspace, srcFeature, heavyRealtion.getLink1(), bodyFeatureFor[0], heavyRealtion.getLink2(), targetFeature );
              else
                command = null;
            }
            else
            {
              final LightRelationType normalRelation = (LightRelationType) relation;
              command = new RemoveRelationCommand( srcFeature, normalRelation.getLink(), targetFeature );
            }
            break;
          default:
            command = null;
        }
        try
        {
          workspace.postCommand( command );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
    } );
  }

  @Override
  public void disposeControl( )
  {
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    m_editRelationViewer = new EditRelationViewer( parent, toolkit, m_data );
    refreshSettings();
    return m_editRelationViewer;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    refreshSettings();
  }

  @Override
  public String getPartName( )
  {
    return null;
  }
}