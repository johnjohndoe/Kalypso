package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureControl;

/**
 * @author belger
 */
public abstract class AbstractFeatureControl implements IFeatureControl
{
  private Feature m_feature;

  private final FeatureTypeProperty m_ftp;
  
  private Collection m_changelisteners = new ArrayList();
  
  public AbstractFeatureControl(  )
  {
    this( null, null );
  }

  public AbstractFeatureControl( final FeatureTypeProperty ftp )
  {
    this( null, ftp );
  }

  public AbstractFeatureControl( final Feature feature, final FeatureTypeProperty ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  public void dispose()
  {
    m_changelisteners.clear();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#getFeature()
   */
  public final Feature getFeature()
  {
    return m_feature;
  }
  
  public final void setFeature( final Feature feature )
  {
    m_feature = feature;
  }
  
  public FeatureTypeProperty getFeatureTypeProperty()
  {
    return m_ftp;
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void addChangeListener( final IFeatureChangeListener l )
  {
    m_changelisteners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeChangeListener(org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public void removeChangeListener( final IFeatureChangeListener l )
  {
    m_changelisteners.remove( l );
  }
  
  protected void fireChange( final FeatureChange change )
  {
    if( change == null )
      return;
    
    for( Iterator iter = m_changelisteners.iterator(); iter.hasNext(); )
      ((IFeatureChangeListener)iter.next()).featureChanged( change );
  }
}
