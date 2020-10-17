function pick_gene(ensembl_id, mut, base_change, chrom, start, end, colour) {
    
    var header = "";
    var table = "";

    row_colour = 'style="background-color:' + colour + '"'
    
    if($('#gene_table').length) {
  
        table = document.getElementById('gene_table');

        table.innerHTML = table.innerHTML + '<tr ' + row_colour + 'id=' + ensembl_id.substring(5)+'><td><a href="http://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=' + ensembl_id + '" target=_blank>' + ensembl_id + ' </a></td><td>'+ mut +'</td><td>'+ base_change +'</td><td>'+ chrom +'</td><td>'+ start +'</td><td>'+ end +'</td><td><input type="button" value="remove" onclick=document.getElementById("'+ensembl_id.substring(5) +'").remove();></td></tr>';
        
    } else {
      
      header = document.createElement('header');
     
      header.className = 'selected_genes_header';
      
      header.id = 'selected_genes_header';
    
      header.innerHTML = '<h4>Selected Genes</h4>';

      table = document.createElement('table');
      
      table.className = 'gene_table';
      
      table.id = 'gene_table';
      
      table.style.width="100%";
      
      table.innerHTML = '<th>Ensembl Gene Id</th><th>Mutation</th><th>Change</th><th>Chr</th><th>Start</th><th>End</th><tr ' + row_colour + ' id='+ensembl_id.substring(5)+'><td><a href="http://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=' + ensembl_id + '" target=_blank>' + ensembl_id + ' </a></td><td>'+ mut +'</td><td>'+ base_change +'</td><td>'+ chrom +'</td><td>'+ start +'</td><td>'+ end +'</td><td><input type="button" value="remove" onclick=document.getElementById("'+ensembl_id.substring(5) +'").remove();></td></tr>';
      
    }

    document.getElementById('genes_div').appendChild(header);
    
    document.getElementById('selected_genes_header').appendChild(table);
}


function show_transcripts(transcripts, ensembl_gene_id, hgnc_gene_name, id, colour) {
  
    var header = "";
    var table = "";
    
    row_colour = 'style="background-color:' + colour + '"'
    
    if($('#transcript_table').length) {
  
        table = document.getElementById('transcript_table');

        table.innerHTML = table.innerHTML +  '<tr ' + row_colour + 'id='+id+'><td>' + hgnc_gene_name + '</td><td>' + ensembl_gene_id  + '</td><td>' + transcripts  + '</td><td><input type="button" value="remove" onclick=document.getElementById("'+id+'").remove();></td></tr>';
        
    } else {

      header = document.createElement('header');
        
      header.className = 'selected_transcripts_header';
      
      header.id = 'selected_transcripts_header';
    
      header.innerHTML = '<h4>Selected Transcripts</h4>';

      table = document.createElement('table');
      
      table.className = 'transcript_table';
      
      table.id = 'transcript_table';

      table.style.width="100%";
      
      table.innerHTML = '<th>HGNC</th><th>Ensembl Gene Id</th><th>Affected Ensembl Transcript Id(s)</th><tr ' + row_colour + ' id=' + id + '><td>&nbsp;' + hgnc_gene_name + '&nbsp;</td><td>&nbsp;' + ensembl_gene_id + '&nbsp;</td><td>' + transcripts  + '</td><td><input type="button" value="remove" onclick=document.getElementById("'+id +'").remove();></td></tr>';
      
    }

    document.getElementById('transcripts_div').appendChild(header);
    
    document.getElementById('selected_transcripts_header').appendChild(table);

}

function show_clinvar(clinvar_text, id, colour) {
  
    var header = "";
    
    var table = "";
    
    row_colour = 'style="background-color:' + colour + '"';
    
    if($('#clinvar_table').length) {
  
        table = document.getElementById('clinvar_table');

        table.innerHTML = table.innerHTML + '<tr ' + row_colour + ' id='+id+'><td>' + clinvar_text + '</td><td><input type="button" value="remove" onclick=document.getElementById("'+id+'").remove();></td></tr>';
        
    } else {

      header = document.createElement('header');
      
      header.className = 'selected_clinvar_header';
      
      header.id = 'selected_clinvar_header';
    
      header.innerHTML = '<h4>ClinVar Records</h4>';
      
      table = document.createElement('table');
      
      clin_table_header = "<th> HGNC | Consequence | Significance | RS Id | Status | Variant Id  </th>";
      
      table.className = 'clinvar_table';
      
      table.id = 'clinvar_table';

      table.style.width="100%";
      
      table.innerHTML = clin_table_header + '<tr ' + row_colour + ' id='+id+'><td>' + clinvar_text  + '</td><td><input type="button" value="remove" onclick=document.getElementById("'+id +'").remove();></td></tr>';
      
    }
    document.getElementById('clinvar_div').appendChild(header);
    
    document.getElementById('selected_clinvar_header').appendChild(table);
}

