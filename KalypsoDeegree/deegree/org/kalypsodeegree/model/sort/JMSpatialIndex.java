package org.deegree.model.sort;

import java.awt.Graphics;
import java.util.List;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree.graphics.transformation.GeoTransform;

public interface JMSpatialIndex
{
    public void add(Object object);

    public void add(GM_Envelope env,Object object);
    public void add(GM_Position pos,Object object);

    public List query(GM_Envelope env,List result);
    public List query(GM_Position env,List result);

    public List queryAll(List result);
    
    public void remove(GM_Envelope env,Object object);
    
    public void remove(Object object);
    
    public void resort(GM_Envelope newEnv,GM_Envelope oldEnv,Object object);
    
    public void resort(GM_Envelope newEnv,Object object);

    public void paint(Graphics g,GeoTransform geoTransform);

    public int rsize();
}
