import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.lang.Math;

/*
 * Created on 27.07.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author kraessig
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class Teilgebiet
{
    private List myBlocks=new ArrayList();
    private Zeitreihe myOutputZeitreihe=new Zeitreihe();
    private final String myKey;
    public Teilgebiet(String key)
    {
        myKey=key;
    }

    public String getKey()
    {
        return myKey;
    }
    public void add(Block block)
    {
        myBlocks.add(block);
    }

       public double getNiederschlag(long time)
    {
        double summe=0;
        for(int i=0;i<myBlocks.size();i++)
        {
            summe+=((Block)myBlocks.get(i)).getNiederschlag(time);

        // Ende Schleife für Niederschlagsaggrgierung zu Gebietsniederschlag auf Tageswerten
        
        }
                
        return Math.round((summe/myBlocks.size())*100)/100.;
        //return summe/myBlocks.size();
    }
    
    public void generateOutput(long time)
    {
        myOutputZeitreihe.add(new Date(time),getNiederschlag(time));
    }
    
    public void writeOutput(File file) throws IOException
    {
       myOutputZeitreihe.exportAsGrap( file);
    }
}
