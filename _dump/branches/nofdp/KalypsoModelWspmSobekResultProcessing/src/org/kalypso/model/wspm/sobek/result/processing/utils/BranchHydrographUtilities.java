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
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.model.wspm.sobek.result.processing.ISobekResultModel;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrograph;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrographModel;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.BranchHydrographHandler;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

public class BranchHydrographUtilities
{

  public static IBranchHydrograph createHydrograph( final ISobekResultModel modelHandler, final CommandableWorkspace workspace, final IBranch branch ) throws CoreException
  {
    try
    {
      final IGMLSchema schema = workspace.getGMLSchema();

      final IFeatureType featureType = schema.getFeatureType( IBranchHydrograph.QN_TYPE );
      final IRelationType relationType = (IRelationType) workspace.getRootFeature().getFeatureType().getProperty( IBranchHydrographModel.QN_HYDROGRAPHS );

      final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

      /* properties */
      final Map<IPropertyType, Object> properties = new HashMap<IPropertyType, Object>();
      properties.put( featureType.getProperty( IBranchHydrograph.QN_BRANCH ), branch.getId() );
      properties.put( featureType.getProperty( IBranchHydrograph.QN_NAME ), branch.getName() );
      properties.put( featureType.getProperty( IBranchHydrograph.QN_PARAM_ID ), "W" );
      properties.put( featureType.getProperty( IBranchHydrograph.QN_UNIT ), "m NHN" );

      final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( workspace, featureType, workspace.getRootFeature(), relationType, -1, properties, selectionManager );
      workspace.postCommand( command );

      final Feature feature = command.getNewFeature();
      final BranchHydrographHandler handler = new BranchHydrographHandler( feature );

      final IObservation<TupleResult> observation = handler.getObservation();
      final TupleResult result = observation.getResult();

      fillObservation( modelHandler, result, branch );

      ObservationFeatureFactory.toFeature( observation, (Feature) feature.getProperty( IBranchHydrograph.QN_RESULT_MEMBER ) );

      return handler;
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  private static void fillObservation( final ISobekResultModel modelHandler, final TupleResult result, final IBranch branch ) throws CoreException
  {
    // ordered in ascending direction
    final ICrossSectionNode[] nodes = branch.getCrossSectionNodes();
    for( final ICrossSectionNode csn : nodes )
    {
      final IProfil profile = csn.getProfile();
      final double station = profile.getStation();

      final IResultTimeSeries timeSeries = modelHandler.getCrossSectionTimeSeries( csn );
      final Double maxValueW = timeSeries.getMaxValue();

      final IRecord record = result.createRecord();
      record.setValue( 0, BigDecimal.valueOf( station ) ); // station
      record.setValue( 1, BigDecimal.valueOf( maxValueW ) ); // w
      record.setValue( 2, BigDecimal.valueOf( 0.0d ) ); // q
      record.setValue( 3, csn.getName() ); // label

      result.add( record );
    }
  }
}
