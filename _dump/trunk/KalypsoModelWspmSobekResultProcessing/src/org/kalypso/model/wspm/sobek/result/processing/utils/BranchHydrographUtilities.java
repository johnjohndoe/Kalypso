package org.kalypso.model.wspm.sobek.result.processing.utils;

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
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrograph;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrographModel;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.BranchHydrographHandler;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

public class BranchHydrographUtilities
{

  public static IBranchHydrograph createHydrograph( final CommandableWorkspace workspace, final IBranch branch ) throws CoreException
  {
    try
    {
      IGMLSchema schema = workspace.getGMLSchema();
      IFeatureType[] allFeatureTypes = schema.getAllFeatureTypes();

      IFeatureType featureType = schema.getFeatureType( IBranchHydrograph.QN_TYPE );
      IRelationType relationType = (IRelationType) workspace.getRootFeature().getFeatureType().getProperty( IBranchHydrographModel.QN_HYDROGRAPHS );

      final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

      /* properties */
      Map<IPropertyType, Object> properties = new HashMap<IPropertyType, Object>();
      properties.put( featureType.getProperty( IBranchHydrograph.QN_BRANCH ), branch.getId() );
      properties.put( featureType.getProperty( IBranchHydrograph.QN_NAME ), branch.getName() );
      properties.put( featureType.getProperty( IBranchHydrograph.QN_PARAM_ID ), "W" );
      properties.put( featureType.getProperty( IBranchHydrograph.QN_UNIT ), "m mHN" );

      final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( workspace, featureType, workspace.getRootFeature(), relationType, -1, properties, selectionManager );
      workspace.postCommand( command );

      Feature feature = command.getNewFeature();
      BranchHydrographHandler handler = new BranchHydrographHandler( feature );

      // FIXME create handler -> getobservation -> fillobservation -> return handler

      return handler;
    }
    catch( Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

}
