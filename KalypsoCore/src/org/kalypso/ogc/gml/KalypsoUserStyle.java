package org.kalypso.ogc.gml;

import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree.model.feature.event.ModellEventProviderAdapter;
import org.deegree.xml.Marshallable;

/**
 * Wrapped UserStyle to provide fireModellEvent Method
 * 
 * @author bce
 */
public class KalypsoUserStyle implements UserStyle, Marshallable, ModellEventProvider
{
  private UserStyle myUserStyle;
  
  private ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  public KalypsoUserStyle( final UserStyle userStyle )
  {
    myUserStyle = userStyle;
  }

  public void addFeatureTypeStyle( FeatureTypeStyle featureTypeStyle )
  {
    myUserStyle.addFeatureTypeStyle( featureTypeStyle );
  }
  public String getAbstract()
  {
    return myUserStyle.getAbstract();
  }
  
  public FeatureTypeStyle[] getFeatureTypeStyles()
  {
    return myUserStyle.getFeatureTypeStyles();
  }
  
  public String getName()
  {
    return myUserStyle.getName();
  }
  
  public String getTitle()
  {
    return myUserStyle.getTitle();
  }
  
  public boolean isDefault()
  {
    return myUserStyle.isDefault();
  }
  
  public void removeFeatureTypeStyle( FeatureTypeStyle featureTypeStyle )
  {
    myUserStyle.removeFeatureTypeStyle( featureTypeStyle );
  }
  
  public void setAbstract( String abstract_ )
  {
    myUserStyle.setAbstract( abstract_ );
  }
  
  public void setDefault( boolean default_ )
  {
    myUserStyle.setDefault( default_ );
  }
  
  public void setFeatureTypeStyles( FeatureTypeStyle[] featureTypeStyles )
  {
    myUserStyle.setFeatureTypeStyles( featureTypeStyles );
  }
  
  public void setName( String name )
  {
    myUserStyle.setName( name );
  }
  
  public void setTitle( String title )
  {
    myUserStyle.setTitle( title );
  }

  public void addModellListener( ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }
  public void fireModellEvent( ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }
  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }

  /**
   * @see org.deegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML()
  {
    return ((Marshallable)myUserStyle).exportAsXML();
  }
}
