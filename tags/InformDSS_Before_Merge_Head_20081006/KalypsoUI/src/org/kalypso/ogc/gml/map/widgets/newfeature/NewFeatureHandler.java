package org.kalypso.ogc.gml.map.widgets.newfeature;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public class NewFeatureHandler extends AbstractHandler implements IHandler
{
  public static final String PARAMETER_FEATURE_TYPE = "org.kalypso.gis.newFeature.type";

  private static final QName GML_LOCATION = new QName( "http://www.opengis.net/gml", "location" );

  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final String parameterFeatureType = event.getParameter( PARAMETER_FEATURE_TYPE );

    final IEvaluationContext applicationContext = (IEvaluationContext) event.getApplicationContext();

    final IMapPanel mapPanel = MapHandlerUtils.getMapPanel( applicationContext );
    final IKalypsoTheme activeTheme = MapHandlerUtils.getActiveTheme( applicationContext );
    if( !(activeTheme instanceof IKalypsoFeatureTheme) )
      throw new ExecutionException( "No feature theme in context" ); //$NON-NLS-1$

    final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) activeTheme;

    final IFeatureType featureType = featureTheme.getFeatureType();
    final FeatureList featureList = featureTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    final IRelationType fatp = featureList.getParentFeatureTypeProperty();
    final CommandableWorkspace workspace = featureTheme.getWorkspace();

    if( fatp == null || parentFeature == null || workspace == null )
      return null;

    final int maxOccurs = fatp.getMaxOccurs();

    /* If we may not inline features we cannot create them via 'new' */
    if( !fatp.isInlineAble() )
      // Just return
      return null;

    /*
     * Direct properties (maxoccurs = 1) can only be added if not already there.
     */
    if( maxOccurs == 1 && parentFeature.getProperty( fatp ) != null )
    {
      // Just return
      return null;
    }

    /*
     * If maxoccurs > 1 we have a list, and we may test if the list is already full.
     */
    else if( maxOccurs > 1 )
    {
      final List< ? > list = (List< ? >) parentFeature.getProperty( fatp );
      if( list != null && list.size() >= maxOccurs )
        return null;
    }

    final IGMLSchema contextSchema = workspace.getGMLSchema();

    final IFeatureType[] featureTypes = GMLSchemaUtilities.getSubstituts( featureType, contextSchema, false, true );
    for( final IFeatureType ft : featureTypes )
    {
      if( ft.getQName().toString().equals( parameterFeatureType ) )
      {
        final IValuePropertyType[] allGeomteryProperties = ft.getAllGeomteryProperties();
        final List<QName> geomPropertiesQName = new ArrayList<QName>();

        for( final IValuePropertyType prop : allGeomteryProperties )
        {
          if( prop.getQName().equals( GML_LOCATION ) )
            continue;
          geomPropertiesQName.add( prop.getQName() );
        }
        final NewFeatureWidget newFeatureWidget = new NewFeatureWidget( ft.getQName(), geomPropertiesQName.toArray( new QName[geomPropertiesQName.size()] ) );
        mapPanel.getWidgetManager().setActualWidget( newFeatureWidget );
      }
    }

    return null;
  }
}
