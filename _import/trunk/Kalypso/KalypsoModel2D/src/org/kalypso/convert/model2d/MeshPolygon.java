/*
 * 
 * This class is for organizing <MeshPolygons>.
 * Each <MeshPolygon consists of three or four edges.
 * 
 * Created on 06.10.2004
 *
 */
package org.kalypso.convert.model2d;

import java.util.Vector;



/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp</a>
 *
 */
public class MeshPolygon {
    
    private int[] countID;
    private Vector vector;
    private MeshPoint mp;
    private boolean exists = false;
    private Vector edgeVector;
    private Edge edge;
    private Edge[] allEdgesOfRect;
    
    /**
     * Constructor
     */
    public MeshPolygon() {
        countID = new int[4];
        vector = new Vector();
        edgeVector = new Vector();
        allEdgesOfRect = new Edge[8];
    }
   
    /**
     * Gets the ids of the feature points. Two feature points 
     * are describing one edge.
     * @return
     */
    public int[] getPointIDs(){
        return countID;
    }
    
    /**
     * 
     * @return vector
     */
    public Vector getVector(){
        return vector;
    }
    
    /**
     * Sets the ids of the <MeshPoint> belonging to a <MeshPolygon>.
     * 
     * @param id
     */
    public void setPoint(int id){
        try{
            mp = new MeshPoint(id);
            
            if (vector.size()== 0){
                setIDs(vector.size(), id);
            } else if (vector.size() == 1){
                setIDs(vector.size(), id);
            } else if (vector.size() == 2){
                setIDs(vector.size(), id);             
            } else if (vector.size() == 3){
                setIDs(vector.size(), id);
            } else if (vector.size() == 4){
                setIDs(vector.size(), id);
            } else System.out.println("vector size is > 4 => no fem! "); 
        }catch(Exception e){
            System.out.println("Error occurred in MeshPolygon: ");
            e.printStackTrace();
        }
    }
    
    /**
     * 
     * @return <Edges> of <MeshPolygon>
     */
    public Edge[] getEdges(){
        return allEdgesOfRect;
    }
    
    /**
     * gets the <Vector> containing the <Edges> of the 
     * particular <MeshPolygon>
     * 
     * @return <Vector> with <Edges> of <MeshPolygon>
     */
    public Vector getEdgesOfRect(){
        return edgeVector;
    }
    
    /**
     * sets AR for defining the edges of the polygon
     * @param id
     * @param p1
     * @param p2
     */
    public void setAR(int id, int p1, int p2){
        edge = new Edge(id, p1, p2);
        try{
            if(edgeVector.size() == 0){
                setEdge(edgeVector.size(), edge);
            } else if (edgeVector.size() == 2){
                setEdge(edgeVector.size(), edge);
            }else if (edgeVector.size() == 4){
                setEdge(edgeVector.size(), edge);
            }else if (edgeVector.size() == 6){
                setEdge(edgeVector.size(), edge);
            } else System.out.println(" edge "+ edge.getID()+" is not fitting for this rectangle ");
            
        }catch(Exception ex){
            System.out.println("Error in setting the edges of the FEM");
            ex.printStackTrace();
        }
        
    }
    
    /**
     * sets the <Edge> at particular position of all edges of the
     * <Mesh> to get the corresponding <MeshPolygon> of the <Edge>
     * 
     * @param pos
     * @param edge
     */
    public void setEdge(int pos, Edge edge){
        int point1 = edge.getP1();
        int point2 = edge.getP2();
        
        allEdgesOfRect[pos] = edge;
        
        int size = pos+1;
        edgeVector.setSize(size);
        edgeVector.add(edge);
    }
    
    /**
     * gets order of the edges of the <MeshPolygon>
     * @return
     */
    public String[] getOrderOfEdges(){
        Edge edge;
        String[] s = new String[8];
        StringBuffer sb = new StringBuffer(1000);
        for(int i = 0; i < edgeVector.size(); i++){
            if(edgeVector.elementAt(i)!= null) {
                edge = (Edge)edgeVector.elementAt(i);
                s[i] = edge.getP1()+","+edge.getP2();
            }
        } 
        
        sb = getOrderOutOfSB(s);
        String[] result = sb.toString().split(","); 

        return result;
    }
    
    /**
     * gets the order of the <Edges> within the <StringBuffer>
     * and creates new <StringBuffer> with the correct order of the 
     * <Edges> of the <MeshPolygon>
     * 
     * @param s
     * @return
     */
    public StringBuffer getOrderOutOfSB(String[] s){
        StringBuffer sb = new StringBuffer(10000);

        for(int j = 0; j < s.length; j++){
            
            if (s[j]!= null){
                String[] tmp = s[j].split(",");
                sb.append(tmp[0]);
                sb.append(",");
                sb.append(tmp[1]);

                break;
            }
        }  
        
        sb = setOrder(s, sb);
        int length = sb.toString().split(",").length;
        if(length != 5){
	        for(int k = 0; k < length; k++){
	            sb = setOrder(s, sb);
	            length = sb.toString().split(",").length;
	            if(length == 5)break;
	        }
        }
        
        return sb;
    }
   
    /**
     * sets the Order of the Edges of the <MeshPolygon>
     * @param s
     * @param sb
     * @return
     */
    public StringBuffer setOrder(String[]s, StringBuffer sb){
        for(int k = 0; k < s.length; k++){

            if (s[k]!= null && !sb.toString().equalsIgnoreCase(s[k])){
                String[] tmp2 = s[k].split(",");
                String s1 = tmp2[0];
                String s2 = tmp2[1];
                
                if(sb.toString().endsWith(s1)){
                    sb.append(","+tmp2[1]);
                } else if(sb.toString().endsWith(s2)){
                    sb.append(","+tmp2[0]);
                }

            }
        }     
        return sb;
    }
    
    /**
     * Sets the corresponding ids
     * @param pos
     * @param id
     */
    public void setIDs(int pos, int id){
            exists = testIDs(id);
            if(exists == false){
                countID[pos] = id;
	            vector.add(mp);                    
            }
    }
    
    /**
     * tests if the particular id is already encountered
     * @param id
     * @return
     */
    public boolean testIDs(int id){        
        for(int i = 0; i < vector.size(); i++){
            if(countID[i] == id){
                exists = true; break;
            } else exists = false;
        }
        return exists;
    }
}
