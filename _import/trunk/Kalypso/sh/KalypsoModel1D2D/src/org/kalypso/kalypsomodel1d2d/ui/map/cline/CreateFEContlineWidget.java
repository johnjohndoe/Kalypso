package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ops.ContinuityLineOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class CreateFEContlineWidget extends AbstractWidget
{
  private Point m_currentPoint = null;

  private LineGeometryBuilder m_builder = null;

  public CreateFEContlineWidget( )
  {
    super( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.0"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    // find the right themes to edit i.e. the discretisation model

    reinit();
  }

  private final void reinit( )
  {
    m_builder = null;

    final CS_CoordinateSystem targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();
    m_builder = new LineGeometryBuilder( 0, targetCrs );
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      if( m_builder != null )
      {
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), m_currentPoint );

        m_builder.addPoint( currentPos );
      }
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    try
    {
      final GM_Curve curve = (GM_Curve) m_builder.finish();

      // validate geometry: doppelte punkte

      FE1D2DDiscretisationModel model = null;
      CommandableWorkspace workspace = null;

      final IKalypsoTheme theme = getMapPanel().getMapModell().getActiveTheme();
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = featureTheme.getFeatureType();
        if( GMLSchemaUtilities.substitutes( 
                    featureType, 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_NODE
                        /*FE1D2DNode.QNAME_FE1D2DNode*/ ) || 
            GMLSchemaUtilities.substitutes( 
                    featureType, 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE
                    /*FE1D2DEdge.QNAME_FE1D2DEdge*/ )|| 
            GMLSchemaUtilities.substitutes( 
                    featureType, 
                    Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement ) )
        {
          final Feature parentFeature = featureTheme.getFeatureList().getParentFeature();

          model = new FE1D2DDiscretisationModel( parentFeature );
          workspace = featureTheme.getWorkspace();
        }
      }

      final IFE1D2DContinuityLine<IFE1D2DComplexElement, IFE1D2DEdge> continuityLine = ContinuityLineOps.contilineFromCurve( curve, model );

      final Feature parentFeature = model.getFeature();
      final IRelationType rt = 
          (IRelationType) parentFeature.getFeatureType().getProperty( 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS);
      final AddFeatureCommand addElementCommand = new AddFeatureCommand( workspace, parentFeature, rt, -1, continuityLine.getWrappedFeature(), null, true );
      workspace.postCommand( addElementCommand );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );

      final IStatus status = StatusUtilities.statusFromThrowable( e );

      final Shell shell = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage().getActivePart().getSite().getShell();
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          ErrorDialog.openError( shell, getName(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.CreateFEContlineWidget.2"), status ); //$NON-NLS-1$
        }
      } );
    }
    finally
    {
      reinit();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

    if( currentPoint != null )
    {
      if( m_builder != null )
        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    }
  }

}
