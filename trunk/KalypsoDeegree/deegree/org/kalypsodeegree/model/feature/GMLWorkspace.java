package org.kalypsodeegree.model.feature;

import java.net.URL;
import java.util.List;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * @author doemming
 */
public interface GMLWorkspace extends ModellEventProvider
{
  public static final int RESOLVE_ALL = 0;

  public static final int RESOLVE_LINK = 1;

  public static final int RESOLVE_COMPOSITION = 2;

  public Feature getRootFeature( );

  public IGMLSchema getGMLSchema( );

  public Feature[] getFeatures( final IFeatureType ft );

  /**
   * Returns the feature that has the given id or null if not found.
   */
  public Feature getFeature( final String id );

  /**
   * resolves the associationlink to a feature, maxOccurs =1
   */
  public Feature resolveLink( final Feature srcFeature, final IRelationType linkProperty );

  /**
   * resolves the associationlink to a feature, maxOccurs =1
   * 
   * @param srcFeature
   * @param linkPropertyName
   * @param resolveMode
   * @return linked feature
   */
  public Feature resolveLink( Feature srcFeature, final IRelationType linkProperty, final int resolveMode );

  /**
   * resolves the associationlink to a feature, maxOccurs >1
   */
  public Feature[] resolveLinks( final Feature srcFeature, final IRelationType linkProperty );

  /**
   * resolves the associationlink to a feature, maxOccurs >1
   * 
   * @param srcFeature
   * @param linkPropertyName
   * @param resolveMode
   * @return features
   */
  public Feature[] resolveLinks( final Feature srcFeature, final IRelationType linkProperty, final int resolveMode );

  /**
   * returns all Features that that link to the linkTargetFeature, with the specified linkPropertyname and are type of
   * linkSourceFeatureType or do substitue it
   */
  public Feature[] resolveWhoLinksTo( Feature linkTargetfeature, IFeatureType linkSrcFeatureType, final IRelationType linkProperty );

  public URL getContext( );

  /** Visit all Features of the given IFeatureType */
  public void accept( final FeatureVisitor fv, final IFeatureType ft, final int depth );

  /** Visit the given feature */
  public void accept( final FeatureVisitor fv, final Feature feature, final int depth );

  /** Visit alle features in the given list */
  public void accept( final FeatureVisitor fv, final List features, final int depth );

  /** Visit alle features denoted by this path */
  public void accept( final FeatureVisitor fv, final String featurePath, final int depth );

  /**
   * @deprecated Retrieve type information via GMLSchema. Use {@link GMLSchema#getFeatureType(QName)} instead.
   */
  @Deprecated
  public IFeatureType getFeatureType( final QName featureQName );

  public Object getFeatureFromPath( final String featurePath );

  public IFeatureType getFeatureTypeFromPath( final String featurePath );

  public FeaturePath getFeaturepathForFeature( final Feature feature );

  public String getSchemaLocationString( );

  /**
   * Creates a feature an puts it into this workspace.
   * <p>
   * Generates a unique id througout this workspace.
   * </p>
   */
  public Feature createFeature( final Feature parent, final IRelationType parentRelation, final IFeatureType type );

  /**
   * Creates a feature an puts it into this workspace. Also create subfeatures where apropriate.
   * <p>
   * Generates a unique id throughout this workspace.
   * </p>
   * 
   * @param depth
   *            Number of levels of subfeatures which shall be created. -1 means infinite, 0 means none (only normal
   *            properties are filled with default values).
   */
  public Feature createFeature( final Feature parent, final IRelationType parentRelation, final IFeatureType type, final int depth );

  /**
   * TODO: commont TODO: we should replace this method by: createAsComposition! First, it is always used as such (that i
   * sfirst created, that this method is called).; Second: a featuree hsould never live without workspace
   * 
   * @param pos
   *            Position at which the new element is inserted into the list. If -1, the new element is added to the end
   *            of the list.
   */
  public void addFeatureAsComposition( Feature parent, final IRelationType linkProperty, int pos, Feature newFeature ) throws Exception;

  public void addFeatureAsAggregation( Feature parent, final IRelationType linkProperty, int pos, String featureID ) throws Exception;

  public void setFeatureAsAggregation( Feature srcFE, final IRelationType linkProperty, int pos, String featureID ) throws Exception;

  public void setFeatureAsAggregation( Feature parent, final IRelationType linkProperty, String featureID, boolean overwrite ) throws Exception;

  /**
   * removes a related feature from the parent. Works only if the child is linked <br>
   * <i>and the relation is not a composition </i> see also
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#removeLinkedAsCompositionFeature(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, org.kalypsodeegree.model.feature.Feature)
   */
  public boolean removeLinkedAsAggregationFeature( final Feature parentFeature, final IRelationType linkProperty, String childFeatureID );

  /**
   * removes a related feature from the parent. Works only if the child is a composition <br>
   * <i>and the relation is not linked </i>
   */
  public boolean removeLinkedAsCompositionFeature( final Feature parentFeature, final IRelationType linkProperty, Feature childFeature );

  /**
   * return true if these feature are related
   */
  public boolean isExistingRelation( final Feature f1, final Feature f2, final IRelationType linkProperty );

  /**
   * @param parent
   * @param linkPropName
   * @param pos
   * @return <code>true</code> if it is a aggregation <br>
   *         <code>false</code> if it is a composition <br>
   *         caution: is link is <code>null</code> return value is undefined
   */
  public boolean isAggregatedLink( Feature parent, final IRelationType linkProperty, int pos );

  /**
   * @param parentFE
   * @param linkPropName
   * @param linkedFE
   * @param overwrite
   * @throws Exception
   */
  public void setFeatureAsComposition( final Feature parentFE, final IRelationType linkProperty, final Feature linkedFE, final boolean overwrite ) throws Exception;

  /**
   * @param visitor
   * @param feature
   * @param depth
   * @param featureProperties
   *            properties to follow
   */
  public void accept( final FeatureVisitor visitor, Feature feature, int depth, final IPropertyType[] featureProperties );

  public boolean contains( final Feature feature );

  public boolean isBrokenLink( final Feature parentFeature, final IPropertyType ftp, final int pos );

  /**
   * @deprecated Retrieve type information via GMLSchema. Use {@link GMLSchema#getFeatureType(QName)} or
   *             {@link GMLSchema#getFeatureType(String)} instead.
   * @deprecated use getFeatureType(QName)
   */
  @Deprecated
  public IFeatureType getFeatureType( final String nameLocalPart );

  /** Return the factory which creates feature providers used to load linked features. */
  public IFeatureProviderFactory getFeatureProviderFactory( );

  /**
   * The namespace context with which this workspace was read from a gm file (if any).<br>
   * May be <code>null</code>.<br>
   * Sometimes needed/necessary, if after reading the complete document, fragments (like xpathes) are going to be
   * evaluated.
   */
  public NamespaceContext getNamespaceContext( );
}