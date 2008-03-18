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
package org.kalypso.model.wspm.sobek.core.model;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IGmlWorkspaces;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.INodeUtils;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.model.wspm.sobek.core.utils.FNGmlUtils;
import org.kalypso.model.wspm.sobek.core.wizard.SobekWizardEditBoundaryNode;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class NodeUtils implements INodeUtils
{
  public static Feature createBoundaryNodeLastfallCondition( final ILastfall lastfall, final IBoundaryNode boundaryNode ) throws Exception
  {
    final IRelationType prop = (IRelationType) boundaryNode.getFeature().getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_LASTFALL_DEFINITION_MEMBER );
    final IFeatureType targetType = prop.getTargetFeatureType();
    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

    final CommandableWorkspace cw = boundaryNode.getModelMember().getWorkspace();
    final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( cw, targetType, boundaryNode.getFeature(), prop, -1, null, selectionManager );
    cw.postCommand( command );

    /* set linked lastfall! */
    final Feature condition = command.getNewFeature();
    FeatureUtils.updateLinkedFeature( cw, condition, ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LINKED_LASTFALL, IGmlWorkspaces.HYDRAUL_MODEL + "#" + lastfall.getFeature().getId() ); //$NON-NLS-1$

    return command.getNewFeature();
  }

  private final ISobekModelMember m_model;

  public NodeUtils( final ISobekModelMember model )
  {
    m_model = model;
  }

  private void boundaryNodeToConnectionNode( final BoundaryNode bn ) throws Exception
  {
    /* create new connection node */
    final IConnectionNode connectionNode = (IConnectionNode) FNGmlUtils.createNode( m_model, TYPE.eConnectionNode, bn.getLocation(), new INode[] {} );

    final Map<QName, Object> map = new HashMap<QName, Object>();
// map.put( ISobekConstants.QN_HYDRAULIC_NAME, bn.getName() );
    map.put( ISobekConstants.QN_HYDRAULIC_DESCRIPTION, bn.getDescription() );

    FeatureUtils.updateFeature( bn.getModel().getWorkspace(), connectionNode.getFeature(), map );

    for( final IBranch branch : bn.getInflowingBranches() )
      connectionNode.addInflowingBranch( branch );

    for( final IBranch branch : bn.getOutflowingBranches() )
      connectionNode.addOutflowingBranch( branch );

    // set node at branches
    final IBranch[] inflowing = connectionNode.getInflowingBranches();
    for( final IBranch branch : inflowing )
      updateBranchNode( branch, connectionNode );

    final IBranch[] outflowing = connectionNode.getOutflowingBranches();
    for( final IBranch branch : outflowing )
      updateBranchNode( branch, connectionNode );

    // delete connection node
    bn.delete();

  }

  private void connectionNodeToBoundaryNode( final IConnectionNode cn ) throws Exception
  {
    /* create new boundary node */
    final IBoundaryNode boundaryNode = (IBoundaryNode) FNGmlUtils.createNode( m_model, TYPE.eBoundaryNode, cn.getLocation(), new INode[] {} );

    final Map<QName, Object> map = new HashMap<QName, Object>();
// map.put( ISobekConstants.QN_HYDRAULIC_NAME, cn.getName() );
    map.put( ISobekConstants.QN_HYDRAULIC_DESCRIPTION, cn.getDescription() );

    FeatureUtils.updateFeature( cn.getModelMember().getWorkspace(), boundaryNode.getFeature(), map );

    for( final IBranch branch : cn.getInflowingBranches() )
      boundaryNode.addInflowingBranch( branch );

    for( final IBranch branch : cn.getOutflowingBranches() )
      boundaryNode.addOutflowingBranch( branch );

    // set node at branches
    final IBranch[] inflowing = boundaryNode.getInflowingBranches();
    for( final IBranch branch : inflowing )
      updateBranchNode( branch, boundaryNode );

    final IBranch[] outflowing = boundaryNode.getOutflowingBranches();
    for( final IBranch branch : outflowing )
      updateBranchNode( branch, boundaryNode );

    new UIJob( Messages.NodeUtils_1 )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        final IWorkbenchWizard wizard = new SobekWizardEditBoundaryNode( boundaryNode );
        wizard.init( PlatformUI.getWorkbench(), null );

        final WizardDialog dialog = new WizardDialog( null, wizard );
        dialog.open();

        final int returnCode = dialog.getReturnCode();
        try
        {
          if( Window.OK == returnCode )
            cn.delete();
          else
            boundaryNode.delete();
        }
        catch( final Exception e )
        {
          return new Status( IStatus.ERROR, this.getClass().getName(), e.getMessage() );
        }

        return Status.OK_STATUS;
      }
    }.schedule();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INodeUtils#switchBoundaryConnectionNode(org.kalypsodeegree.model.feature.Feature)
   */
  public void switchBoundaryConnectionNode( final Feature node ) throws Exception
  {
    final QName nqn = node.getFeatureType().getQName();

    /* which node type? */
    if( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE.equals( nqn ) )
      connectionNodeToBoundaryNode( new ConnectionNode( m_model, node ) );
    else if( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE.equals( nqn ) )
      boundaryNodeToConnectionNode( new BoundaryNode( m_model, node ) );
    else
      throw new NotImplementedException();
  }

  private void updateBranchNode( final IBranch branch, final INode node ) throws Exception
  {
    final GM_Curve curve = branch.getGeometryProperty();
    final GM_Point pn = node.getLocation();

    if( curve.getStartPoint().intersects( pn ) )
      branch.setUpperNode( node );
    else if( curve.getEndPoint().intersects( pn ) )
      branch.setLowerNode( node );
  }
}
