/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
 
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
Contact:
 
Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de
 
Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.wts;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.media.j3d.AmbientLight;
import javax.media.j3d.Background;
import javax.media.j3d.BoundingSphere;
import javax.media.j3d.DirectionalLight;
import javax.media.j3d.Group;
import javax.media.j3d.Light;
import javax.media.j3d.Shape3D;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;
import javax.vecmath.Vector3f;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree.services.wts.AtmosphericCondition;
import org.deegree.services.wts.ViewPoint;
import org.deegree.services.wts.WTSScene;
import org.deegree_impl.services.wts.util.SunLight;
import org.deegree_impl.services.wts.util.SunPosition;


/**
 * The <tt>WTSScene_Impl</tt> represents the basic class for creation of a 3D
 * terrain model in the sense of the OGC Web Terrain Service specification. A
 * WTS scene is defined by a terrain model and a date determining the light 
 * condions. Additional elements are 3D or 2.5D-features that are placed into
 * the scene, atmospheric conditions influencing the light and visibility (e.g.
 * fog, rain etc.) and additional light placed into the scene (e.g. street lights, 
 * spots, lighted windows etc.).
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:lupp@lat-lon.de">Katharina Lupp</a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class WTSScene_Impl implements WTSScene {
    
    private Calendar calendar               = null;
    private ArrayList conditions            = null;
    private ArrayList feature               = null;
    private ArrayList terrain               = null;
    private ArrayList lights                = null;
    private ViewPoint viewPoint             = null;
    private Object background               = null;
    private GM_Envelope envelope            = null;
    
    /**
     * Creates a new instance of WTSScene_Impl
     * @param terrain Shape3D object(s) representing the scenes terrain.
     * @param feature feature that shall be published in the scene
     * @param viewPoint object that describes the viewers position and the point 
     *                  he looks at
     */
    public WTSScene_Impl(Shape3D[] terrain, Group[] feature, ViewPoint viewPoint) {
        this(terrain, feature, viewPoint, null);        
    }
    
    /** Creates a new instance of WTSScene_Impl
     * @param terrain Shape3D object(s) representing the scenes terrain.
     * @param feature feature that shall be published in the scene
     * @param viewPoint object that describes the viewers position and the point
     *                  he looks at
     * @param calendar describtion of the date and time for which the scene shall 
     *                 be rendered --> light
     * conditions
     */
    public WTSScene_Impl(Shape3D[] terrain, Group[] feature, ViewPoint viewPoint, 
                         Calendar calendar) {
        this(terrain, feature, viewPoint, calendar, null);
    }
    
    /** Creates a new instance of WTSScene_Impl
     * @param terrain Shape3D object(s) representing the scenes terrain.
     * @param feature feature that shall be published in the scene
     * @param viewPoint object that describes the viewers position and the point 
     *                  he looks at
     * @param calendar describtion of the date and time for which the scene shall 
     *                  be rendered --> light
     * conditions
     * @param conditions atmospheric conditions like fog, dust, rain etc. of the scene
     */
    public WTSScene_Impl(Shape3D[] terrain, Group[] feature, ViewPoint viewPoint, 
                         Calendar calendar, AtmosphericCondition[] conditions) {
        this(terrain, feature, viewPoint, calendar, conditions, null);
    }
    
    /** Creates a new instance of WTSScene_Impl
     * @param terrain Shape3D object(s) representing the scenes terrain.
     * @param feature feature that shall be published in the scene
     * @param viewPoint object that describes the viewers position and the point 
     *                  he looks at
     * @param calendar describtion of the date and time for which the scene shall 
     * be rendered --> light conditions
     * @param conditions atmospheric conditions like fog, dust, rain etc. of the scene
     * @param lights lights in addition to sun and ambient light (e.g. street lights, 
     * spots etc.)
     */
    public WTSScene_Impl(Shape3D[] terrain, Group[] feature, ViewPoint viewPoint, 
                         Calendar calendar, AtmosphericCondition[] conditions, 
                         Light[] lights) {
        this(terrain, feature, viewPoint, calendar, conditions, lights, null);
    }        
    
    /** Creates a new instance of WTSScene_Impl
     * @param terrain Shape3D object(s) representing the scenes terrain.
     * @param feature feature that shall be published in the scene
     * @param viewPoint object that describes the viewers position and the point 
     *                  he looks at
     * @param calendar describtion of the date and time for which the scene shall 
     *                 be rendered --> light conditions
     * @param conditions atmospheric conditions like fog, dust, rain etc. of the scene
     * @param lights lights in addition to sun and ambient light (e.g. street lights, spots
     * etc.)
     * @param background scene background; have to be a <tt>Shape3D</tt> or a 
     *                   <tt>Background</tt>
     */
    public WTSScene_Impl(Shape3D[] terrain, Group[] feature, ViewPoint viewPoint, 
                         Calendar calendar, AtmosphericCondition[] conditions, 
                         Light[] lights, Object background) {
        if ( feature != null ) {
            this.feature = new ArrayList( feature.length );
        } else {
            this.feature = new ArrayList();
        }        
        if ( lights != null ) {
            this.lights = new ArrayList(lights.length);        
        } else {
            this.lights = new ArrayList();        
        }
        this.conditions = new ArrayList();
        this.terrain = new ArrayList();        
        
        setTerrain( terrain );
        setViewPoint( viewPoint );
        setFeatures (feature); 
        setDate (calendar);
        setAtmosphericConditions (conditions);
        if ( background instanceof Background ) {
            setBackground( (Background)background );
        } else {
            setBackground( background );
        }
        setLights( lights );
        //createDayLight();
        //calcBoundingBox();
    }
    
    
    /**
     * creates the light that results from the sun (direct light) and the ambient
     * of the sky.
     */
    private void createDayLight() {
        
        int latitute = 50;
        int year = calendar.get( Calendar.YEAR ); 
        int month = calendar.get( Calendar.MONTH )+1;
        int date = calendar.get( Calendar.DAY_OF_MONTH );
        int hour = calendar.get( Calendar.HOUR_OF_DAY );
        int minute = calendar.get( Calendar.MINUTE );
        
        Vector3f vec = SunLight.calculateSunlight( latitute, year, month, date, hour, minute, 0 );
        double vPos = SunPosition.calcVerticalSunposition( latitute, year, month, date, hour, minute );
        double hPos = SunPosition.calcHorizontalSunPosition( hour, minute );
        
        Color3f white = new Color3f( vec );

        ViewPoint vp = getViewPoint();
        Point3d p = vp.getObserverPosition();
        Point3d origin = new Point3d( -p.x, p.y, p.z );      
        BoundingSphere light_bounds = new BoundingSphere(origin, 250000);
        
        //Directional Light: A DirectionalLight node defines an oriented light with an origin at infinity.
        DirectionalLight headlight = new DirectionalLight( );        
        headlight.setInfluencingBounds( light_bounds );
        headlight.setColor( white );
        headlight.setDirection( (float)Math.sin( hPos ),
                                -(float)Math.sin( vPos ),
                                (float)Math.abs( Math.cos( hPos ) ) );
        lights.add( headlight );
        //Ambient Light: Ambient light is that light that seems to come from all directions. 
        //Ambient light has only an ambient reflection component. 
        //It does not have diffuse or specular reflection components. 
        AmbientLight al = new AmbientLight();        
        al.setInfluencingBounds(light_bounds);
        al.setColor( new Color3f( 0.5f*vec.x, 0.5f*vec.y, 0.5f*vec.z) );
        
        lights.add( al );
    }

     /** returns the &lt;Format&gt; element
      * @return the features published into the scene
      */
     public Group[] getFeatures() {
        return (Group[])feature.toArray( new Group[feature.size()] );
    }
    
     /** sets the features that shall be rendered. the first feature of the array
      * shall be the digital elevation model
      * @param feature features to publish into the scene
      */
     public void setFeatures(Group[] features) {
        this.feature.clear();
        if ( feature!= null ) {
            for (int i = 0; i < features.length; i++) {
                this.feature.add( features[i] );
            }
        }
    }
        
    /** adds a feature that shall be rendered. the first feature to be set
     * shall be the digital elevation model
     * @param feature feature to publish into the scene
     */
    public void addFeature(Group feature) {
        this.feature.add(feature);
    }
    
     /** returns the background object of the scene. this may be a <tt>Background</tt>,
     * a <tt>Shape3D</tt> or <tt>null</tt> if no background is defined.
     *
     */
    public Object getBackground() {
        return background; 
    }
     
    /** sets the <tt>Background</tt> object of the scene
     *
     */
    public void setBackground(Background background) {
        this.background = background;
    }
    
    /** sets the background of the scene as <tt>Object</tt>
     *
     */
    public void setBackground(Object background) {
        this.background = background;
    }
       
    /** gets the atmospheric conditions for the rendering.
     * @return atmospheric conditions of the scene
     */
    public AtmosphericCondition[] getAtmosphericConditions() {
        AtmosphericCondition[] tmp = new AtmosphericCondition[conditions.size()];
        return (AtmosphericCondition[])conditions.toArray(tmp);
    }
   
    /** sets the atmospheric conditions for the rendering. e.g. if a clear
     * day (maybe summer late morning) is assumed there will be very sharp
     * shadows. if vice versa a cloudy day (let's say autumn late afternoon)
     * with some poor rain and a bit fog is assumed there won't be sharp shadows
     * but some kind of 'gray curtain' over the scene.
     * <p>
     * at the moment it isn't specified how to define atmospheric conditions
     * in a standarized form so just an <tt>Object</tt> is submitted to the
     * method.
     * @param conditions atmospheric conditions of the scene like fog, rain etc.
     */
    public void setAtmosphericConditions(AtmosphericCondition[] conditions) {
       this.conditions.clear();
       if ( conditions != null ) {
           for (int i = 0; i < conditions.length; i++) {
               addAtmosphericCondition( conditions[i] );
           }
       }
    }
    
    /** adds a atmospheric condition to the scene.<p>
     * @see setAtmosphericConditions(AtmosphericCondition[])
     * @param condition atmospheric conditions of the scene like fog, rain etc.
     */
    public void addAtmosphericCondition(AtmosphericCondition condition) {
        this.conditions.add( condition );
    }

    /** get the date and the time for determining time depending the light conditions
     * of the scene
     * @return describtion of the date and time for which the scene shall be rendered --> light
     * conditions
     */
    public Calendar getDate (){
        return calendar;
    }
    
    /** set the date and the time for determining time depending the light conditions
     * of the scene
     * @param calendar describtion of the date and time for which the scene shall be rendered --> light
     * conditions
     */
    public void setDate(Calendar calendar) {
        if ( calendar == null ) {
            // 15.06.2002 12°°
            calendar = new GregorianCalendar(2000, 6, 15, 12, 0);
        }
        this.calendar = calendar;
        // TODO
        // set light conditions for date and time
    }
              
    
    /** sets the terrain of the WTS-Scene as Shape3D object containing an appearence
     * @param feature Shape3D object representing the scenes terrain.
     */
    public void setTerrain(Shape3D feature) {
        terrain.clear();
        if ( feature != null ) {
            terrain.add( feature );
        }
    }
    
    /** sets the terrain of the WTS-Scene as set of several independ Shape3D
     * objects containing an appearence
     * @param features Shape3D objects representing the scenes terrain.
     */
    public void setTerrain(Shape3D[] features) {
        terrain.clear();
        if ( feature != null ) {
            for (int i = 0; i < features.length; i++) {
                terrain.add( features[i] );
            }
        }
    }
        
    
    /** returns the features that constructs the terrain model of the scene
     * @return Shape3D object(s) representing the scenes terrain.
     */
    public Shape3D[] getTerrain() {
        Shape3D[] tmp = new Shape3D[ terrain.size() ];
        return (Shape3D[])terrain.toArray( tmp );
    }
    
    /** gets the position of the viewer, the directions he looks and his field
     * of view in radians
     * @return object that describes the viewers position and the point he looks at
     */
    public ViewPoint getViewPoint() {
        return viewPoint;
    }
    
    /** defines the position of the viewer and the point he looks at.
     * @param viewPoint object that describes the viewers position and the point he looks at
     */
    public void setViewPoint(ViewPoint viewPoint) {
        this.viewPoint = viewPoint;
    }
    
    /** adds a light to the scene. this can be ambient, directional and
     * point light.
     * @param light a light in addition to sun and basic ambient light (e.g. 
     * street lights, spots etc.)
     */
    public void addLight(Light light) {
        this.lights.add( light );
    }
    
    /** returns the lights of the scene
     * @return lights including sun and basic ambient light (e.g. street 
     * lights, spots etc.)
     */
    public Light[] getLights() {
        return (Light[])lights.toArray( new Light[ lights.size() ] );
    }
     
    /** sets the lights of the scene. this can be ambient, directional and
     * point light.
     * @param lights lights in addition to sun and basic ambient light 
     * (e.g. street lights, spots etc.)
     */
    public void setLights(Light[] lights) {
        this.lights.clear();
        setDate( calendar );
        createDayLight();
        if ( lights != null ) {
            for (int i = 0; i < lights.length; i++) {
                addLight( lights[i] );
            }
        }
    }    

    /** returns the envelope od the scene. 
     */
    public GM_Envelope getEnvelope() {
        return envelope;
    }    
    
    /**
     * returns the four corner coordinates of frame the viewer sees next to him
     * and that contains data
     */
    public GM_Position[] getFrontBorderFrame() {
        return null;
    }
    
    /** returns the four corner coordinates of farest frame the viewer sees
     * and that contains data
     *
     */
    public GM_Position[] getBackFrame() {
        return null;
    }
    
}
