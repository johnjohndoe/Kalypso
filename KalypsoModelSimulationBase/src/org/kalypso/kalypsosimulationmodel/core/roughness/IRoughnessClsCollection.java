package org.kalypso.kalypsosimulationmodel.core.roughness;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogRoughness;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IModel;

/**
 * Interface to be implemented by classes representing a wbr:RoughnessCollection elemenent
 * 
 * @author Patrice Congo
 */
public interface IRoughnessClsCollection extends IModel
{
  // roughness collection
  public static final QName WBR_F_ROUGHNESS_CLS_COLLECTION = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "RoughnessClsCollection" ); //$NON-NLS-1$

  public static final QName WBR_PROP_ROUGHNESS_CLS_MEMBER = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "roughnessClsMember" ); //$NON-NLS-1$


  /**
   * Select all roughness in the collection with a name matching the the given regular expression
   * 
   * @param nameRegExp
   *          --
   * @return
   */
  public List<IRoughnessCls> selectRoughnessByName( String nameRegExp );

  public IFeatureBindingCollection<IRoughnessCls> getRoughnessClasses( );
}
