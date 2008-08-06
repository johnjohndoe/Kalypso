package org.kalypso.model.wspm.sobek.result.processing.utils;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode.STRUCTURE_TYPE;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.ISobekResultModel;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWeirNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchLengthSection;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchLengthSectionModel;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.BranchLengthSectionHandler;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

public class BranchLengthSectionUtilities
{

  public static IBranchLengthSection createLengthSection( final ISobekResultModel modelHandler, final CommandableWorkspace workspace, final IBranch branch ) throws CoreException
  {
    try
    {
      final IGMLSchema schema = workspace.getGMLSchema();

      final IFeatureType featureType = schema.getFeatureType( IBranchLengthSection.QN_TYPE );
      final IRelationType relationType = (IRelationType) workspace.getRootFeature().getFeatureType().getProperty( IBranchLengthSectionModel.QN_HYDROGRAPHS );

      final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

      /* properties */
      final Map<IPropertyType, Object> properties = new HashMap<IPropertyType, Object>();
      properties.put( featureType.getProperty( IBranchLengthSection.QN_BRANCH ), branch.getId() );
      properties.put( featureType.getProperty( IBranchLengthSection.QN_NAME ), branch.getName() );
      properties.put( featureType.getProperty( IBranchLengthSection.QN_PARAM_ID ), "W" ); //$NON-NLS-1$
      properties.put( featureType.getProperty( IBranchLengthSection.QN_UNIT ), "m NHN" ); //$NON-NLS-1$

      final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( workspace, featureType, workspace.getRootFeature(), relationType, -1, properties, selectionManager );
      workspace.postCommand( command );

      final Feature feature = command.getNewFeature();
      final BranchLengthSectionHandler handler = new BranchLengthSectionHandler( feature );

      final IObservation<TupleResult> observation = handler.getObservation();
      final TupleResult result = observation.getResult();

      fillObservation( modelHandler, result, branch );

      ObservationFeatureFactory.toFeature( observation, (Feature) feature.getProperty( IBranchLengthSection.QN_RESULT_MEMBER ) );

      return handler;
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  /**
   * <pre>
   * Structure of an result branch
   * 
   *  CN--------CSN----CSN----Empty-----CSN-----CSN---------CN
   * </pre>
   */
  private static void fillObservation( final ISobekResultModel modelHandler, final TupleResult result, final IBranch branch ) throws CoreException
  {
    INode[] nodes = branch.getNodes();
    for( INode node : nodes )
    {
      if( node instanceof IConnectionNode || node instanceof ICrossSectionNode )
      {
        try
        {
          IResultTimeSeries timeseries = modelHandler.getNodeTimeSeries( node );
          if( timeseries == null )
          {
            continue;
          }

          Double maxValue = timeseries.getMaxValue();
          Double position = timeseries.getPositionOnBranch( branch );
          IFeatureType featureType = node.getFeature().getFeatureType();

          final IRecord record = result.createRecord();

          record.setValue( 0, BigDecimal.valueOf( position ) ); // station
          record.setValue( 1, BigDecimal.valueOf( maxValue ) ); // w
          record.setValue( 2, BigDecimal.valueOf( 0.0d ) ); // q
          record.setValue( 3, node.getName() ); // label
          record.setValue( 4, featureType.getQName().getLocalPart() );

          result.add( record );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }

      }
      else if( node instanceof IEmptyNode )
      {
        IEmptyNode empty = (IEmptyNode) node;

        // only weir nodes contains waterlevels
        STRUCTURE_TYPE type = empty.getStructureType();
        if( STRUCTURE_TYPE.eWeir.equals( type ) )
        {
          IWeirNodeResultWrapper wrapper = modelHandler.getWeirNodeResult( empty );

          IRecord recordBelow;
          try
          {
            /* above */
            IResultTimeSeries above = wrapper.getWaterLevelAbove();
            IFeatureType featureType = node.getFeature().getFeatureType();

            final IRecord recordAbove = result.createRecord();
            recordAbove.setValue( 0, BigDecimal.valueOf( above.getPositionOnBranch( branch ) - 0.1 ) ); // station
            recordAbove.setValue( 1, BigDecimal.valueOf( above.getMaxValue() ) ); // w
            recordAbove.setValue( 2, BigDecimal.valueOf( 0.0d ) ); // q
            recordAbove.setValue( 3, node.getName() ); // label
            recordAbove.setValue( 4, featureType.getQName().getLocalPart() );

            result.add( recordAbove );

            /* below */
            IResultTimeSeries below = wrapper.getWaterLevelBelow();

            recordBelow = result.createRecord();
            recordBelow.setValue( 0, BigDecimal.valueOf( below.getPositionOnBranch( branch ) + 0.1 ) ); // station
            recordBelow.setValue( 1, BigDecimal.valueOf( below.getMaxValue() ) ); // w
            recordBelow.setValue( 2, BigDecimal.valueOf( 0.0d ) ); // q
            recordBelow.setValue( 3, node.getName() ); // label
            recordBelow.setValue( 4, featureType.getQName().getLocalPart() );

            result.add( recordBelow );
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }
        }
      }
    }
  }
}
