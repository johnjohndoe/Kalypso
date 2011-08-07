package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.afgui.model.Util;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeIFeatureWrapper2NameCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Thomas Jung
 * 
 */
final class AssignNodeCellModifier implements ICellModifier
{
  private final TableViewer m_nodeElevationViewer;

  private final ApplyElevationWidgetDataModel m_dataModel;

  AssignNodeCellModifier( final TableViewer nodeElevationViewer, final ApplyElevationWidgetDataModel dataModel )
  {
    m_nodeElevationViewer = nodeElevationViewer;
    m_dataModel = dataModel;
  }

  @Override
  public boolean canModify( final Object element, final String property )
  {
    // Find the index of the column
    final Object[] properties = m_nodeElevationViewer.getColumnProperties();
    return property.equals( properties[0] ) || property.equals( properties[1] );
  }

  @Override
  public Object getValue( final Object element, final String property )
  {
    if( property.equals( m_nodeElevationViewer.getColumnProperties()[1] ) )
      return FENodeHeightProvider.getElevationString( (IFE1D2DNode< ? >) element );
    else if( property.equals( m_nodeElevationViewer.getColumnProperties()[0] ) )
      return FENodeNameProvider.getNameOrID( (IFE1D2DNode< ? >) element );
    else
      return null;
  }

  @Override
  public void modify( final Object element, final String property, final Object value )
  {
    final IFE1D2DNode< ? > node;
    if( element instanceof TableItem )
    {
      final Object data = ((TableItem) element).getData();
      node = (data instanceof IFE1D2DNode) ? (IFE1D2DNode< ? >) data : null;
    }
    else
      return;

    if( property.equals( m_nodeElevationViewer.getColumnProperties()[1] ) )
    {
      if( FENodeHeightProvider.getElevationString( node ).equals( value ) )
        return;

      final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
      if( model1d2d == null )
        return;

      /* Parse value */
      final double newElevation;
      if( StringUtils.isEmpty( (String) value ) )
      {
        /* Empty string allows to delete elevation */
        newElevation = Double.NaN;
      }
      else
      {
        newElevation = NumberUtils.parseQuietDouble( (String) value );
        if( Double.isNaN( newElevation ) )
          return;
      }

      final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );

      final ChangeNodePositionCommand command = new ChangeNodePositionCommand( model1d2d, node, newElevation, false )
      {
        @Override
        public void process( ) throws Exception
        {
          super.process();

          refreshTable( node, property, true );
        }
      };
      try
      {
        workspace.postCommand( command );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    else if( property.equals( m_nodeElevationViewer.getColumnProperties()[0] ) )
    {
      if( FENodeNameProvider.getNameOrID( node ).equals( value ) )
        return;

      final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );

      final ChangeIFeatureWrapper2NameCmd cmd = new ChangeIFeatureWrapper2NameCmd( node, (String) value )
      {
        @Override
        public void process( ) throws Exception
        {
          super.process();

          refreshTable( node, property, false );
        }
      };
      try
      {
        workspace.postCommand( cmd );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    else
      System.out.println( "BAD property:" + property ); //$NON-NLS-1$
  }

  protected void refreshTable( final IFE1D2DNode< ? > node, final String property, final boolean invalidateMap )
  {
    if( invalidateMap )
    {
      final IMapPanel mapPanel = m_dataModel.getMapPanel();
      mapPanel.invalidateMap();
    }

    ViewerUtilities.update( m_nodeElevationViewer, new Object[] { node }, new String[] { property }, true );
  }
}