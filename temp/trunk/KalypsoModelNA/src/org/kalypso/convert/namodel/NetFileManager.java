package org.kalypso.convert.namodel;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Writer;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public class NetFileManager extends AbstractManager
{

    private final FeatureType m_nodeFT;
    private final FeatureType m_vChannelFT;
    private final FeatureType m_mkChannelFT;
    private final FeatureType m_catchmentFT;
    
    public NetFileManager(GMLSchema schema,NAConfiguration conf) throws IOException
    {
        super(conf.getNetFormatURL());
        m_nodeFT=schema.getFeatureType("Node");
        m_vChannelFT=schema.getFeatureType("VirtualChannel");
        m_mkChannelFT=schema.getFeatureType("KMChannel");
        m_catchmentFT=schema.getFeatureType("Catchment");
     }

    public String mapID(int id, FeatureType ft)
    {
        return ft.getName()+id;        
    }

    public Feature[] parseFile(URL url) throws Exception
    {
        LineNumberReader reader =new LineNumberReader( new InputStreamReader(url.openConnection().getInputStream()));
        HashMap nodeCollector=new HashMap();
        readNet(reader,nodeCollector);
        Collection valueCol = nodeCollector.values();
        return (Feature[]) valueCol.toArray(new Feature[valueCol.size()]);
        //        while( (fe=readNextFeature( reader ))!=null)
//          result.add(fe);
//           return (Feature[])result.toArray(new Feature[result.size()]);
        
    }

    private void readNodeList(LineNumberReader reader) throws Exception
    {
       HashMap propCollector = new HashMap();
       String line;
       line=reader.readLine();
       if(line==null || line.startsWith("9999"))
           return;
       System.out.println( 3 + ": " + line );   
       createProperties(propCollector,line,3);
       final FeatureProperty knotProp = (FeatureProperty)propCollector.get( "knot" );
       final FeatureProperty izugProp = (FeatureProperty)propCollector.get( "izug" );
       final FeatureProperty iabgProp = (FeatureProperty)propCollector.get( "iabg" );
       final FeatureProperty iuebProp = (FeatureProperty)propCollector.get( "iueb" );
       final FeatureProperty izufProp = (FeatureProperty)propCollector.get( "izuf" );
       final FeatureProperty ivzwgProp = (FeatureProperty)propCollector.get( "ivzwg" );
       int knot = Integer.parseInt( (String)knotProp.getValue() );
       int izug = Integer.parseInt( (String)izugProp.getValue() );
       int iabg = Integer.parseInt( (String)iabgProp.getValue() );
       int iueb = Integer.parseInt( (String)iuebProp.getValue() );
       int izuf = Integer.parseInt( (String)izufProp.getValue() );
       int ivzwg = Integer.parseInt( (String)ivzwgProp.getValue() );
       final Feature fe=getFeature(knot,m_nodeFT);
       if(izug>0)
       {
           //TODO...
       }
    }
    private void readNet(LineNumberReader reader,HashMap nodeCollector) throws Exception
    {
       HashMap propCollector = new HashMap();
       String line;
       line=reader.readLine();
      if(line==null || line.startsWith("9999"))
          return;
       if(line.startsWith("\\"))
       {
           readNet(reader,nodeCollector);
           return;
       }
       System.out.println( 0 + ": " + line );   
       createProperties(propCollector,line,0);
       final FeatureProperty iteilProp = (FeatureProperty)propCollector.get( "iteil" );
       final FeatureProperty istrngProp = (FeatureProperty)propCollector.get( "istrng" );
       final FeatureProperty iknotoProp = (FeatureProperty)propCollector.get( "iknoto" );
       final FeatureProperty iknotuProp = (FeatureProperty)propCollector.get( "iknotu" );
       int iteil = Integer.parseInt( (String)iteilProp.getValue() );
       int istrngNr = Integer.parseInt( (String)istrngProp.getValue() );
       int iknotoNr = Integer.parseInt( (String)iknotoProp.getValue() );
       int iknotuNr = Integer.parseInt( (String)iknotuProp.getValue() );
       // create node feature and
       // set node numbers
       final FeatureProperty numPropertyKnotO = FeatureFactory.createFeatureProperty("num",""+iknotoNr);
       final Feature knotoFE=getFeature(iknotoNr,m_nodeFT);
       nodeCollector.put(knotoFE.getId(),knotoFE);
       knotoFE.setProperty(numPropertyKnotO);
       final FeatureProperty numPropertyKnotU = FeatureFactory.createFeatureProperty("num",""+iknotuNr);
       final Feature knotuFE=getFeature(iknotuNr,m_nodeFT);
       nodeCollector.put(knotuFE.getId(),knotuFE);
       knotuFE.setProperty(numPropertyKnotU);
       // set node channel relations
       final Feature strangFE=getExistingFeature(istrngNr,new FeatureType[]{m_mkChannelFT,m_vChannelFT});
       // node -> strang
       if(strangFE==null)
           System.out.println(istrngNr);
       //
       else
       {
       final FeatureProperty downStreamProp1=FeatureFactory.createFeatureProperty("downStreamChannelMember",strangFE.getId());
       knotoFE.setProperty(downStreamProp1); 
       // strang -> node
       final FeatureProperty downStreamProp2=FeatureFactory.createFeatureProperty("downStreamNodeMember",knotuFE.getId());
       strangFE.setProperty(downStreamProp2); 
       
       
       // Teilgebiete lesen
       for(int i=0;i<iteil;i++)
       {
           line=reader.readLine();
           final HashMap col=new HashMap();
           System.out.println( 1 + ": " + line );   
           createProperties(col,line,1);
           final FeatureProperty nteilProp = (FeatureProperty)col.get( "nteil" );
           final int nteil = Integer.parseInt( (String)nteilProp.getValue() );
           final Feature teilgebFE=getFeature(nteil,m_catchmentFT);    
           final FeatureProperty downStreamProp=FeatureFactory.createFeatureProperty("entwaesserungsStrangMember",strangFE.getId());
           teilgebFE.setProperty(downStreamProp);
       }
       //
       }
       readNet(reader,nodeCollector);
    }
    
    /* (non-Javadoc)
     * @see org.kalypso.convert.namodel.AbstractManager#writeFile(java.io.Writer, org.deegree.model.feature.GMLWorkspace)
     */
    public void writeFile(Writer writer, GMLWorkspace workspace)
            throws Exception
    {
        // TODO Auto-generated method stub

    }

}
